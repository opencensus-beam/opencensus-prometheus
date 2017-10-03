-module(oc_prometheus_reporter).

-include_lib("opencensus/include/opencensus.hrl").

-behaviour(oc_reporter).

-export([init/1,
         report/2]).

%% ===================================================================
%% API
%% ===================================================================

init(_) ->
  prometheus_summary:declare([{name, "span"},
                              {labels, [name]},
                              {help, "Opencensus span"}]),
  ok.

report(Spans, _Opts) ->
  [
   prometheus_summary:observe("span", [Span#span.name],
                              wts:duration(Span#span.start_time, Span#span.end_time))

   || Span <- Spans],
  ok.


%% ===================================================================
%% Private functions
%% ===================================================================
