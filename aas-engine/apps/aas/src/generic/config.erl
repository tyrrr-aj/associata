-module(config).
-export([init/0, store/3, get/2]).


init() -> ets:new(agds_config, [set, protected, {read_concurrency, true}]).


store(Key, Value, Config) -> ets:insert(Config, {Key, Value}).


get(Key, Config) ->
    [{Key, Value}] = ets:lookup(Config, Key),
    Value.