# lager_logstash 0.1.2 [![Build Status][travis_ci_image]][travis_ci]

[Lager][lager] backend for sending logs to [Logstash][logstash].

Includes `lager_logstash_json_formatter` which can be used with other
`lager` backends.

## Configuration

Add `lager_logstash` to your `rebar.config` deps:

``` erlang
{deps,
 [
  {lager_logstash, "",
   {git, "https://github.com/rpt/lager_logstash.git",
    {tag, "0.1.2"}}}
 ]}.
```

Remember to also add `jsx` or `jiffy`, whichever you prefer.

And finally, configure `lager` app with something like this:

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
].
```

## Features

  * outputs: `tcp`, `udp`, `file`
  * formats: `json`
  * json encoders: `jsx`, `jiffy`

## JSON formatter

Here's how you would use the included JSON formatter with the
`lager_file_backend`:

``` erlang
{lager_file_backend,
 [
  {file, "log/lager_logstash.log"},
  {level, info},
  {formatter, lager_logstash_json_formatter},
  {formatter_config, [{json_encoder, jsx}]},
  {size, 10485760},
  {date, "$D0"},
  {count, 5}
 ]}
```

## TODOs

  * reconnect
  * other formats

## Changelog

#### 0.1.2

  * Removes all release name/version logging code

#### 0.1.1

  * Adds `lager_logstash_json_formatter`


[travis_ci]: https://travis-ci.org/rpt/lager_logstash
[travis_ci_image]: https://travis-ci.org/rpt/lager_logstash.png
[lager]: https://github.com/basho/lager
[logstash]: http://logstash.net
