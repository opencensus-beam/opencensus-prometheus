-module(oep_SUITE).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-include_lib("opencensus/include/opencensus.hrl").

all() ->
  [prometheus_reporter_default,
   prometheus_reporter_labels,
   prometheus_reporter_histogram].

init_per_suite(Config) ->
  ok = application:load(opencensus),
  Config.

end_per_suite(_Config) ->
  ok.

init_per_testcase(prometheus_reporter_default, Config) ->
  application:set_env(opencensus, reporter, {oc_prometheus_reporter, []}),
  {ok, _} = application:ensure_all_started(opencensus_erlang_prometheus),
  Config;
init_per_testcase(prometheus_reporter_labels, Config) ->
  application:set_env(opencensus, reporter,
                      {oc_prometheus_reporter, [{name, span_with_labels},
                                                {labels, [op_name, {common_name, <<"cname">>}]}]}),
  {ok, _} = application:ensure_all_started(opencensus_erlang_prometheus),
  Config;
init_per_testcase(prometheus_reporter_histogram, Config) ->
  application:set_env(opencensus, reporter,
                      {oc_prometheus_reporter, [{type, histogram},
                                                {name, span_histogram_seconds},
                                                {buckets, [0, 1, 2]}]}),
  {ok, _} = application:ensure_all_started(opencensus_erlang_prometheus),
  Config.

end_per_testcase(_, _Config) ->
  ok = application:stop(opencensus_erlang_prometheus),
  ok = application:stop(opencensus),
  ok = application:stop(prometheus),
  ok.

%% ===================================================================
%% Tests
%% ===================================================================

prometheus_reporter_default(_Config) ->

  SpanName1 = <<"span-1">>,
  Span1 = opencensus:start_span(SpanName1, opencensus:generate_trace_id(), undefined),

  ChildSpanName1 = <<"child-span-1">>,
  ChildSpan1 = opencensus:start_span(ChildSpanName1, Span1),
  opencensus:finish_span(ChildSpan1),
  opencensus:finish_span(Span1),

  timer:sleep(1000),

  ?assertMatch({1, _}, prometheus_summary:value(span, [SpanName1])),
  ?assertMatch({1, _}, prometheus_summary:value(span, [ChildSpanName1])).

prometheus_reporter_labels(_Config) ->

  SpanName1 = <<"span-1">>,
  Span1 = opencensus:start_span(SpanName1, opencensus:generate_trace_id(), #{<<"op_name">> => "test"}),
  
  ChildSpanName1 = <<"child-span-1">>,
  ChildSpan1 = opencensus:start_span(ChildSpanName1, Span1, #{<<"cname">> => "github"}),
  
  opencensus:finish_span(ChildSpan1),
  opencensus:finish_span(Span1),

  timer:sleep(1000),

  ?assertMatch({1, _}, prometheus_summary:value(span_with_labels, [SpanName1, "test", "N/A"])),
  ?assertMatch({1, _}, prometheus_summary:value(span_with_labels, [ChildSpanName1, "N/A", "github"])).

prometheus_reporter_histogram(_Config) ->

  SpanName1 = <<"span-1">>,
  Span1 = opencensus:start_span(SpanName1, opencensus:generate_trace_id(), #{<<"op_name">> => "test"}),
  
  ChildSpanName1 = <<"child-span-1">>,
  ChildSpan1 = opencensus:start_span(ChildSpanName1, Span1, #{<<"cname">> => "github"}),
  timer:sleep(1500),
  opencensus:finish_span(ChildSpan1),
  timer:sleep(1000),
  opencensus:finish_span(Span1),

  timer:sleep(1000),

  ?assertMatch({[0,0,0,1], _}, prometheus_histogram:value(span_histogram_seconds, [SpanName1])),
  ?assertMatch({[0,0,1,0], _}, prometheus_histogram:value(span_histogram_seconds, [ChildSpanName1])).

%% ===================================================================
%% Private functions
%% ===================================================================
