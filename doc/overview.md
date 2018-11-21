@copyright 2018 Ilya Khaprov <<i.khaprov@gmail.com>>.
@title Opencensus Prometheus integration for Erlang/BEAM
@version 0.3.2

@doc

[![Hex.pm][Hex badge]][Hex link]
[![Hex.pm Downloads][Hex downloads badge]][Hex link]
[![Build Status][Travis badge]][Travis link]
[![Coverage Status][Coveralls badge]][Coveralls link]

> [Opencensus][Opencensus Erlang link]: Erlang stats collection and distributed tracing framework.

> [Prometheus.io][Prometheus Erlang link]: monitoring system and time series database client in Erlang.

## Using

Opencensus has trace and stats packages, trace package if for distributed tracing and
stats package is for backend-agnostic metrics collection.

This library implements a trace `reporter' for exporting spans duration as prometheus metrics and
a stats `exporter' for exporting metrics collected using Opencensus.

Example configuration:

<pre lang="erlang">
{opencensus, [
        {reporter, {oc_reporter_sequential, [
          {oc_reporter_zipkin, [
            {url, "http://localhost:9411"},
            {service_name, "service_name"}
          ]},
          {oc_prometheus_reporter, [{type, histogram}, %% metric type
                                    {name, span_histogram_seconds}, %% metric name, note the time unit
                                    {buckets, [0, 1, 2]}]} %% histogram buckets with bounds in the time unit
        ]}},
        {sampler, {oc_sampler_probability, [
            {probability, 0.001} %% one in a thousand
        ]}}
    ]}
</pre>

Since Prometheus uses push model you don't need to set up Opencensus stats exporter as usual.
Instead, this library provides `oc_stat_exporter_prometheus' which implements Prometheus collector interface.

<pre lang="erlang">
  prometheus_registry:register_collector(oc_stat_exporter_prometheus)
</pre>

## Other Prometheus and Opencensus Links

## Opencensus

- [Cowboy 2 Integration](https://github.com/deadtrickster/opencensus-cowboy)
- [Google Cloud Trace](https://github.com/tsloughter/oc_google_reporter)

### Prometheus

- [Cowboy1/2 Exporters and Cowboy2 instrumenter](https://hex.pm/packages/prometheus_cowboy)
- [Ecto Instrumenter](https://hex.pm/packages/prometheus_ecto)
- [Elixir client](https://github.com/deadtrickster/prometheus.ex)
- [Elixir plugs Instrumenters and Exporter](https://hex.pm/packages/prometheus_plugs)
- [Extatus - App to report metrics to Prometheus from Elixir GenServers](https://github.com/gmtprime/extatus)
- [Fuse plugin](https://github.com/jlouis/fuse#fuse_stats_prometheus)
- [Inets HTTPD Exporter](https://github.com/deadtrickster/prometheus_httpd)
- [OS process info Collector](https://hex.pm/packages/prometheus_process_collector) (linux, freebsd, macos)
- [Phoenix Instrumenter](https://hex.pm/packages/prometheus_phoenix)
- [RabbitMQ Exporter](https://github.com/deadtrickster/prometheus_rabbitmq_exporter).

## Contributing

Section order:

- Types
- Macros
- Callbacks
- Public API
- Deprecations
- Private Parts

Install the `git' pre-commit hook:

<pre lang="bash">
./bin/pre-commit.sh install
</pre>

The pre-commit check can be skipped by passing `--no-verify' to `git commit'.

## License

MIT

<!-- Named Links -->

[Hex badge]: https://img.shields.io/hexpm/v/opencensus_erlang_prometheus.svg?maxAge=2592000?style=plastic
[Hex link]: https://hex.pm/packages/opencensus_erlang_prometheus
[Hex downloads badge]: https://img.shields.io/hexpm/dt/opencensus_erlang_prometheus.svg?maxAge=2592000
[Travis badge]: https://travis-ci.org/deadtrickster/opencensus_erlang_prometheus.svg?branch=version-3
[Travis link]: https://travis-ci.org/deadtrickster/opencensus_erlang_prometheus
[Coveralls badge]: https://coveralls.io/repos/github/deadtrickster/opencensus_erlang_prometheus/badge.svg?branch=master
[Coveralls link]: https://coveralls.io/github/deadtrickster/opencensus_erlang_prometheus?branch=master
[Opencensus Erlang link]: https://github.com/census-instrumentation/opencensus-erlang
[Prometheus Erlang link]: https://github.com/deadtrickster/prometheus.erl
