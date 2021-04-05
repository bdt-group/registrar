# registrar

[![Build Status](https://travis-ci.com/bdt-group/registrar.svg?branch=main)](https://travis-ci.com/bdt-group/registrar)

A collection of global-like process registries

## Overview

The project provides a collection of modules suitable for usage
as `via` modules in `gen_server`, `gen_statem` and so on. Thus,
every module in the project provides 4 global-like API functions:

- `register_name/2`
- `unregister_name/1`
- `whereis_name/1`
- `send/2`

Refer to [global](https://erlang.org/doc/man/global.html) module documentation
for details.

Also, every module provides `start/0` and `stop/0` functions. Note that,
`Module:start/0` function **MUST** be called prior to use of any registrar module `Module`.

**NOTE**: despite it's a normal situation when a registered process dies unexpectedly,
for performance reasons it's still highly recommended to call `unregister_name/1`
function explicitly on process termination (e.g. inside `terminate/2,3` callback
of `gen_server` or `gen_statem`).

## Registrar modules

Currently two modules are available: `registrar_gproc` and `registrar_local`.

### registrar_gproc

The module uses `gproc` application as process registry. It's not very efficient
because it relies on an intermediate relay process for maintaining concurrency and
dealing with dead processes (using monitors). Even though `gproc` is able to deal with global
process registry, current implementation only uses local `gproc` registry,
i.e. a registered process is only accessible by it's name on local node only.

As stated in [Overview](#overview), you must call `registrar_gproc:start()` prior
to use of the module.

**NOTE**: you must add `gproc` dependency explicitly to your project if you want
to use this registrar module.

### registrar_local

The module is intended to be used for local process registry only, i.e. a registered
process is only accessible by it's name on local node only. The process registry is
kept inside ETS table. The module doesn't use any intermediate relay processes:
concurrency is based on atomicity of `ets:insert_new/2` function. To deal with
dead processes two anti-entropy mechanisms are used:

- A background process is running, that traverses the underlying ETS table periodically
  to clean up dead processes. The interval is controlled by `clean_interval` option of
  `start/1` function (see below).
- Read-Repair: during lookup, the registered process is checked for liveness. If it's
  dead, it will be deleted from the underlying ETS table by the caller.

As stated in [Overview](#overview), you must call `registrar_local:start()` prior
to use of the module.

Besides `start/0` the module also exports `start/1` function:
```erl
-spec start(Options :: options()) -> ok | {error, term()}.
```
where `Options` are defined as:
```erl
-type options() :: #{clean_interval => millisecs()}.
-type millisecs() :: non_neg_integer().
```
`clean_interval` option sets the interval that is used by background process
for periodic registry cleanup from dead processes (see above). The default
value is `60000` milliseconds (i.e. `1` minute).
