-module(utils).

-export([get_timestamp_str/0]).


get_timestamp_str() ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:local_time(),
    io_lib:format(
        "~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w", 
        [Year, Month, Day, Hour, Minute, Second]
    ).