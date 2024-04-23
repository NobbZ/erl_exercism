# erl_exercism

A small helper library, that is used in [`exercism`] Erlang exercises, to easily
swap out any remote calls into the module under test to a remote call into the
example implementation if this becomes necessary, eg. under the repositories CI.

[`exercism`]: https://exercism.org/

To use it, you have to include it into your test module. This will enable a
parse transformation, which rewrites the mentioned remote calls. Please see the
modules description for a detailed description of the workflow and algorithms.

## Build

```
$ rebar3 compile
```

## Tests

```
$ rebar3 eunit
```

## Documentation

```
$ rebar3 ex_doc
```
