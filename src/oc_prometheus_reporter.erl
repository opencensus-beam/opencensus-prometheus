-module(oc_prometheus_reporter).

-include_lib("opencensus/include/opencensus.hrl").

-behaviour(oc_reporter).

-export([init/1,
         report/2]).

%% ===================================================================
%% API
%% ===================================================================

init(Config) ->
  Type = config_metric_type(Config),
  Name = config_metric_name(Config),
  Labels = config_metric_labels(Config),
  {LabelNames, Attributes} = lists:unzip(Labels),
  Type:declare([{name, Name},
                {labels, [name] ++ LabelNames},
                {buckets, config_metric_buckets(Config)},
                {help, "Opencensus span metric"}]),
  {Type, Name, Attributes}.

report(Spans, Opts) ->
  [observe_span(Span, Opts) || Span <- Spans],
  ok.

%% ===================================================================
%% Private functions
%% ===================================================================

config_metric_type(Config) ->
  type_to_module(proplists:get_value(type, Config, summary)).

config_metric_name(Config) ->
  proplists:get_value(name, Config, span).

config_metric_buckets(Config) ->
  proplists:get_value(buckets, Config, default).

config_metric_labels(Config) ->
  [normalize_label(Label)
   || Label <- proplists:get_value(labels, Config, [])].

normalize_label(Name) when is_atom(Name) ->
  NameStr = atom_to_binary(Name, utf8),
  {NameStr, NameStr};
normalize_label(Name) when is_binary(Name) ->
  {Name, Name};
normalize_label(Name) when is_list(Name) ->
  {Name, list_to_binary(Name)};
normalize_label({Label, Attribute}) when is_binary(Attribute) ->
  {Label, Attribute}.

compute_labels(Span, Attributes) ->
  Map = Span#span.attributes,
  [maps:get(Attribute, Map, "N/A") || Attribute <- Attributes].

observe_span(Span, {MetricType, MetricName, Attributes}) ->
  Labels = [Span#span.name] ++ compute_labels(Span, Attributes),
  observe_span(MetricType, MetricName, Labels, Span).

observe_span(prometheus_summary, MetricName, Labels, Span) ->
  prometheus_summary:observe(MetricName, Labels, span_duration(Span));
observe_span(prometheus_histogram, MetricName, Labels, Span) ->
  prometheus_histogram:observe(MetricName, Labels, span_duration(Span));
observe_span(prometheus_counter, MetricName, Labels, _Span) ->
  prometheus_counter:inc(MetricName, Labels).

type_to_module(counter) ->
  prometheus_counter;
type_to_module(summary) ->
  prometheus_summary;
type_to_module(histogram) ->
  prometheus_histogram;
type_to_module(Type) ->
  Type.

-spec span_duration(opencensus:span()) -> integer().
span_duration(#span{start_time={StartTime,_}, end_time={EndTime,_}}) ->
    EndTime - StartTime.
