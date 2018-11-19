-module(oc_stat_exporter_prometheus).

-export([collect_mf/2,
         deregister_cleanup/1]).

-behaviour(prometheus_collector).

collect_mf(_Registry, Callback) ->
  ViewDatas = oc_stat:export(),
  [Callback(view_data_to_mf(ViewData)) || ViewData <- ViewDatas],
  ok.

deregister_cleanup(_Registry) ->
  ok.

-spec view_data_to_mf(oc_stat_view:view_data()) ->
                         prometheus_model:'MetricFamily'().
view_data_to_mf(#{name := Name,
                  description := Description,
                  ctags := CTags,
                  tags := Tags,
                  data := #{type := Type,
                            rows := Rows}}) ->
  FullRows = augment_rows_tags(Rows, Tags, CTags),
  Metrics = rows_to_metrics(Type, FullRows),
  prometheus_model_helpers:create_mf(sanitize(Name), Description, to_prom_type(Type), Metrics).

augment_rows_tags(Rows, Tags, CTags) ->
  [{maps:to_list(maps:merge(CTags, maps:from_list(lists:zip(Tags, TagsV)))), Value}
   || #{tags := TagsV, value := Value} <- Rows].

rows_to_metrics(latest, Rows) ->
  prometheus_model_helpers:gauge_metrics(Rows);
rows_to_metrics(count, Rows) ->
  prometheus_model_helpers:counter_metrics(Rows);
rows_to_metrics(sum, Rows) ->
  [prometheus_model_helpers:summary_metric(Tags, Count, Sum)
   || {Tags, #{count := Count, sum := Sum}} <- Rows];
rows_to_metrics(distribution, Rows) ->
  [prometheus_model_helpers:histogram_metric(
     Tags, sum_bucket_counters(Buckets), Count, Sum)
   || {Tags, #{count := Count, sum := Sum, buckets := Buckets}} <- Rows].

sum_bucket_counters([{Bound, Start} | Counters]) ->
  sum_bucket_counters(Counters, [{Bound, Start}], Start).

sum_bucket_counters([], LAcc, _CAcc) ->
  LAcc;
sum_bucket_counters([{Bucket, Counter} | Counters], LAcc, CAcc) ->
  sum_bucket_counters(Counters, LAcc ++ [{Bucket, CAcc + Counter}], CAcc + Counter).

to_prom_type(latest) ->
  gauge;
to_prom_type(count) ->
  counter;
to_prom_type(sum) ->
  summary;
to_prom_type(distribution) ->
  histogram.

%% replace all non-alphanumeric characters with underscores
sanitize(String) ->
    case re:replace(String, "[^[:alpha:][:digit:]]+", "_", [global]) of
        [$_ | _]=S ->
            ["key", S];
        [D | _]=S when D >= 48 andalso D =< 57->
            ["key_", S];
        S ->
            S
    end.
