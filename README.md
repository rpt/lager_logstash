# lager_logstash 0.1.0 [![Build Status][travis_ci_image]][travis_ci]

[Lager][lager] backend for sending logs to [Logstash][logstash].

## Configuration

``` erlang
[
 {lager,
  [
   {handlers,
    [
     {lager_logstash_backend,
      [
       {level, info},
       {output, {tcp, "localhost", 5000}},
       %% {output, {udp, "localhost", 5000}},
       %% {output, {file, "/var/log/lager_logstash.log"}},
       {format, json},
       {json_encoder, jsx}
      ]}
    ]}
  ]}
]
```

## Features

  * outputs: `tcp`, `udp`, `file`
  * formats: `json`
  * json encoders: `jsx`, `jiffy`

## TODOs

  * reconnect
  * other formats

[travis_ci]: https://travis-ci.org/rpt/lager_logstash
[travis_ci_image]: https://travis-ci.org/rpt/lager_logstash.png
[lager]: https://github.com/basho/lager
[logstash]: http://logstash.net
