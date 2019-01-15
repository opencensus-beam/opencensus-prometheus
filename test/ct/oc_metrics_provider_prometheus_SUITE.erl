-module(oc_metrics_provider_prometheus_SUITE).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

all() ->
  [prometheus_provider_default].

init_per_suite(Config) ->
  _ = application:load(opencensus),
  _ = application:ensure_all_started(opencensus_erlang_prometheus),
  Config.

end_per_suite(_Config) ->
  ok.

prometheus_provider_default(_Config) ->
  SpanName1 = <<"span-1">>,
  Span1 = oc_trace:start_span(SpanName1, undefined),

  ?assertMatch(qwe, oc_producer_registry:read_to_list()),

  ChildSpanName1 = <<"child-span-1">>,
  ChildSpan1 = oc_trace:start_span(ChildSpanName1, Span1, #{}),

  ?assertMatch(qwe, oc_producer_registry:read_to_list()),
  oc_trace:finish_span(ChildSpan1),
  oc_trace:finish_span(Span1).
