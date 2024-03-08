# AAS Engine

## Build

Associata uses a custom wrapper around rebar3 to build the engine part of the project. Custom files for rebar3 can be found in the rebar3-custom directory.

To use this customized version of rebar3, all those files need to be copied or symlinked to the PATH, preferably to C:\Program Files\Erlang OTP\bin, **replacing** the original `rebar3.cmd` file found in this directory.