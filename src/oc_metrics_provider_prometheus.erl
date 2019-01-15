-module(oc_metrics_provider_prometheus).

-export([collect_mf/2,
         deregister_cleanup/1]).

-include_lib("opencensus/include/oc_metrics.hrl").

-behaviour(prometheus_collector).

collect_mf(_Registry, Callback) ->
  OCRegistry = default, %% TODO: make it configurable
  oc_producer_registry:read_all(OCRegistry, fun(OCMetric) ->
                                                Callback(oc_metric_to_prom_mf(OCMetric))
                                            end),
  ok.

deregister_cleanup(_Registry) ->
  ok.

oc_metric_to_prom_mf(#oc_metric{descriptor=#oc_metric_descriptor{
                                              name=Name,
                                              description=Description,
                                              type=Type,
                                              label_keys=LabelKeys
                                             },
                                timeseries=Timeseries
                               }) ->

  PromType = to_prom_type(Type),
  Labels = [Key || #oc_label_key{key=Key} <- LabelKeys],
  Metrics = timeseries_to_metrics(PromType, Labels, Timeseries),
  prometheus_model_helpers:create_mf(sanitize(Name), Description, PromType, Metrics).

timeseries_to_metrics(Type, Labels, Timeseries) ->
  [timeseries_to_metric(Type, Labels, T) || T <- Timeseries].

timeseries_to_metric(Type, Labels, #oc_time_series{
                                     label_values=LabelValues,
                                     points=Points
                                    }) ->
  if
    length(Points) > 1 -> erlang:error("More than one point in timeseries isn't supported by Prometheus exporter.");
    t -> t
  end,

  [#oc_point{
      value = PointValue}] = Points,

  Labels = lists:zip(Labels,
                     [Value || #oc_label_value{value = Value} <- LabelValues]),

  value_to_metric(Type, Labels, PointValue).

value_to_metric(untyped, Labels, Value) when is_number(Value) ->
  prometheus_model_helpers:untyped_metric(Labels, Value);
value_to_metric(gauge, Labels, Value) when is_number(Value) ->
  prometheus_model_helpers:gauge_metric(Labels, Value);
value_to_metric(counter, Labels, Value) when is_number(Value) ->
  prometheus_model_helpers:counter_metric(Labels, Value);
value_to_metric(summary, Labels, #oc_summary{count=Count,
                                             sum=Sum}) ->
  prometheus_model_helpers:summary_metric(Labels, Count, Sum);
value_to_metric(histogram, Labels, #oc_distribution{count=Count,
                                                    sum=Sum,
                                                    bucket_options=undefined}) ->
  prometheus_model_helpers:summary_metric(Labels, Count, Sum);
value_to_metric(histogram, Labels, #oc_distribution{count=Count,
                                                    sum=Sum,
                                                    bucket_options=#oc_explicit_bucket_options{
                                                                      bounds=Bounds},
                                                    buckets=Buckets}) ->
  BCounters = [BCount || #oc_bucket{count=BCount} <- Buckets],
  prometheus_model_helpers:historgram_metric(Labels,
                                             sum_bucket_counters(lists:zip(Bounds, BCounters)),
                                             Count,
                                             Sum).

%% augment_rows_tags(Rows, Tags, CTags) ->
%%   [{maps:to_list(maps:merge(CTags, maps:from_list(lists:zip(Tags, TagsV)))), Value}
%%    || #{tags := TagsV, value := Value} <- Rows].

sum_bucket_counters([{Bound, Start} | Counters]) ->
  sum_bucket_counters(Counters, [{Bound, Start}], Start).

sum_bucket_counters([], LAcc, _CAcc) ->
  LAcc;
sum_bucket_counters([{Bucket, Counter} | Counters], LAcc, CAcc) ->
  sum_bucket_counters(Counters, LAcc ++ [{Bucket, CAcc + Counter}], CAcc + Counter).

to_prom_type('UNSPECIFIED') ->
  untyped;
to_prom_type('GAUGE_INT64') ->
  gauge;
to_prom_type('GAUGE_DOUBLE') ->
  gauge;
to_prom_type('GAUGE_DISTRIBUTION') ->
  erlang:error("GAUGE_DISTRIBUTION isn't supported by prometheus exporter");
to_prom_type('CUMULATIVE_INT64') ->
  counter;
to_prom_type('CUMULATIVE_DOUBLE') ->
  counter;
to_prom_type('CUMULATIVE_DISTRIBUTION') ->
  histogram;
to_prom_type('SUMMARY') ->
  summary.

%% replace all non-alphanumeric characters with underscores
sanitize(String) ->
  case re:replace(String, "[^[:alpha:][:digit:]:]+", "_", [global]) of
    [$_ | _]=S ->
      ["key", S];
    [D | _]=S when D >= 48 andalso D =< 57->
      ["key_", S];
    S ->
      S
  end.
