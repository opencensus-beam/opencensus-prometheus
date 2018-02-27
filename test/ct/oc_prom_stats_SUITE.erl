-module(oc_prom_stats_SUITE).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-include_lib("opencensus/include/opencensus.hrl").

all() ->
  [
   {group, positive}
  ].

groups() ->
  [
   {positive, [sequential], [
                             full
                            ]}
  ].

init_per_suite(Config) ->
  Config.

end_per_suite(_Config) ->
  ok.

init_per_testcase(_Name, Config) ->
  {ok, _} = application:ensure_all_started(opencensus_erlang_prometheus),
  Config.

end_per_testcase(_, _Config) ->
  ok = application:stop(opencensus_erlang_prometheus),
  ok = application:stop(opencensus),
  ok = application:stop(prometheus),
  ok.
%% ===================================================================
%% TESTS
%% ===================================================================

full(_Config) ->
  prometheus_registry:clear(),

  ok = oc_stat_view:subscribe(
         "video_count",
         "number of videos processed processed over time",
         [#{ctag => value},
          type],
         'my.org/measures/video_count',
         oc_prometheus_counter),

  ok = oc_stat_view:subscribe(
         "video_sum",
         "video_size_sum",
         [#{sum_tag => value},
          type, category],
         'my.org/measures/video_size_sum',
         oc_prometheus_summary),

  ok = oc_stat_view:subscribe(
         "video_size",
         "number of videos processed processed over time",
         [#{ctag => value}],
         'my.org/measures/video_size_sum',
         {oc_prometheus_histogram, [{buckets, [0, 1 bsl 16, 1 bsl 32]}]}),

  ok = oc_stat_view:subscribe(
         "last_video_size",
         "last processed video size",
         [#{ctag => value}],
         'my.org/measures/video_size_sum',
         oc_prometheus_gauge),

  Ctx = oc_tags:new_ctx(ctx:new(), #{type => "mpeg",
                                     category => "category1"}),

  oc_stat:record('my.org/measures/video_count', Ctx, 1),
  oc_stat:record('my.org/measures/video_count', Ctx, 1),
  oc_stat:record('my.org/measures/video_size_sum', Ctx, 1024),
  oc_stat:record('my.org/measures/video_size_sum', Ctx, 4096),


  ?assertMatch(2, prometheus_counter:value("video_count", ["mpeg"])),
  ?assertMatch({2, 5120}, prometheus_summary:value("video_sum", ["category1", "mpeg"])),
  ?assertMatch({[0, 2, 0, 0], 5120}, prometheus_histogram:value("video_size", [])),
  ?assertMatch(4096, prometheus_gauge:value("last_video_size", [])),

  ?assertMatch(<<"# TYPE video_count counter
# HELP video_count number of videos processed processed over time
video_count{ctag=\"value\",type=\"mpeg\"} 2
# TYPE video_sum summary
# HELP video_sum video_size_sum
video_sum_count{sum_tag=\"value\",category=\"category1\",type=\"mpeg\"} 2
video_sum_sum{sum_tag=\"value\",category=\"category1\",type=\"mpeg\"} 5120
# TYPE video_size histogram
# HELP video_size number of videos processed processed over time
video_size_bucket{ctag=\"value\",le=\"0\"} 0
video_size_bucket{ctag=\"value\",le=\"65536\"} 2
video_size_bucket{ctag=\"value\",le=\"4294967296\"} 2
video_size_bucket{ctag=\"value\",le=\"+Inf\"} 2
video_size_count{ctag=\"value\"} 2
video_size_sum{ctag=\"value\"} 5120
# TYPE last_video_size gauge
# HELP last_video_size last processed video size
last_video_size{ctag=\"value\"} 4096

">>, prometheus_text_format:format()).
