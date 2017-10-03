-module(oep_SUITE).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-include_lib("opencensus/include/opencensus.hrl").

all() ->
  [prometheus_reporter].

init_per_suite(Config) ->
  Config.

end_per_suite(_Config) ->
  ok.

%% ===================================================================
%% Tests
%% ===================================================================

prometheus_reporter(_Config) -> 
  application:set_env(opencensus, reporter, {oc_prometheus_reporter, []}),
  ok = application:load(opencensus),
  application:set_env(opencensus, reporter, {oc_prometheus_reporter, []}),
  {ok, _} = application:ensure_all_started(opencensus_erlang_prometheus),

  SpanName1 = <<"span-1">>,
  Span1 = opencensus:start_span(SpanName1, opencensus:generate_trace_id(), undefined),

  ChildSpanName1 = <<"child-span-1">>,
  ChildSpan1 = opencensus:start_span(ChildSpanName1, Span1),
  opencensus:finish_span(ChildSpan1),
  opencensus:finish_span(Span1),

  timer:sleep(1000),

  ?assertMatch({1, _}, prometheus_summary:value("span", [SpanName1])),
  ?assertMatch({1, _}, prometheus_summary:value("span", [ChildSpanName1])).

%% ===================================================================
%% Private functions
%% ===================================================================
