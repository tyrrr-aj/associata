-module(utils).

-export([get_timestamp_str/0]).


get_timestamp_str() ->
    Nanoseconds = erlang:system_time(nanosecond),
    MegaSecs = Nanoseconds div 1000000000000000,
    Secs = (Nanoseconds div 1000000000) rem 1000000,
    Nanos = Nanoseconds rem 1000000000,
    {{Year, Month, Day}, {Hour, Minute, Second}} = 
        calendar:now_to_universal_time({MegaSecs, Secs, 0}),
    lists:flatten(io_lib:format(
        "~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B:~9..0B",
        [Year, Month, Day, Hour, Minute, Second, Nanos]
    )).