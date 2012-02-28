-module(rfc3339).

-export([parse/1]).

parse(TimestampString) ->
    parse_timestamp_rfc3339(TimestampString, 0, [], []).

%% @doc rfc3339 parser
parse_timestamp_rfc3339(Timestamp, N, A, R) ->
    parse_timestamp_rfc3339(Timestamp, N, A, R, date_fullyear).

parse_timestamp_rfc3339([$-|Cs], 4, A, R, date_fullyear) ->
    parse_timestamp_rfc3339(Cs, 0, [], [{date_fullyear, lists:reverse(A)}|R], date_month);
parse_timestamp_rfc3339([C|Cs], N, A, R, date_fullyear) when N < 4  ->
    parse_timestamp_rfc3339(Cs, N+1, [C|A], R, date_fullyear);
parse_timestamp_rfc3339([_C|_Cs], N, _A, _R, date_fullyear) when N >= 4 ->
    {parse_error, "date_fullyear contains too many digits"};

parse_timestamp_rfc3339([$-|Cs], 2, A, R, date_month) ->
    parse_timestamp_rfc3339(Cs, 0, [], [{date_month, lists:reverse(A)}|R], date_mday);
parse_timestamp_rfc3339([C|Cs], N, A, R, date_month) when N < 2  ->
    parse_timestamp_rfc3339(Cs, N+1, [C|A], R, date_month);
parse_timestamp_rfc3339([_C|_Cs], N, _A, _R, date_month) when N >= 2 ->
    {parse_error, "date_month contains too many digits"};

parse_timestamp_rfc3339([$T|Cs], 2, A, R, date_mday) ->
    parse_timestamp_rfc3339(Cs, 0, [], [{date_mday, lists:reverse(A)}|R], time_hour);
parse_timestamp_rfc3339([C|Cs], N, A, R, date_mday) when N < 2  ->
    parse_timestamp_rfc3339(Cs, N+1, [C|A], R, date_mday);
parse_timestamp_rfc3339([_C|_Cs], N, _A, _R, date_mday) when N >= 2 ->
    {parse_error, "date_mday contains too many digits"};

parse_timestamp_rfc3339([$:|Cs], 2, A, R, time_hour) ->
    parse_timestamp_rfc3339(Cs, 0, [], [{time_hour, lists:reverse(A)}|R], time_minute);
parse_timestamp_rfc3339([C|Cs], N, A, R, time_hour) when N < 2  ->
    parse_timestamp_rfc3339(Cs, N+1, [C|A], R, time_hour);
parse_timestamp_rfc3339([_C|_Cs], N, _A, _R, time_hour) when N >= 2 ->
    {parse_error, "time_hour contains too many digits"};

parse_timestamp_rfc3339([$:|Cs], 2, A, R, time_minute) ->
    parse_timestamp_rfc3339(Cs, 0, [], [{time_minute, lists:reverse(A)}|R], time_second);
parse_timestamp_rfc3339([C|Cs], N, A, R, time_minute) when N < 2  ->
    parse_timestamp_rfc3339(Cs, N+1, [C|A], R, time_minute);
parse_timestamp_rfc3339([_C|_Cs], N, _A, _R, time_minute) when N >= 2 ->
    {parse_error, "time_minute contains too many digits"};

parse_timestamp_rfc3339([$.|Cs], 2, A, R, time_second) ->
    parse_timestamp_rfc3339(Cs, 0, [], [{time_second, lists:reverse(A)}|R], time_secfrac);
parse_timestamp_rfc3339([$+|Cs], 2, A, R, time_second) ->
    parse_timestamp_rfc3339(Cs, 0, [], [{time_second, lists:reverse(A)}|R], time_numoffset_hour);
parse_timestamp_rfc3339([$-|Cs], 2, A, R, time_second) ->
    parse_timestamp_rfc3339(Cs, 0, [], [{time_second, lists:reverse(A)}|R], time_numoffset_hour);
parse_timestamp_rfc3339([$Z|_Cs], 2, A, R, time_second) ->
    lists:reverse([{time_second, lists:reverse(A)}|R]);
parse_timestamp_rfc3339([C|Cs], N, A, R, time_second)     when N < 2  ->
    parse_timestamp_rfc3339(Cs, N+1, [C|A], R, time_second);
parse_timestamp_rfc3339([_C|_Cs], N, _A, _R, time_second) when N >= 2 ->
    {parse_error, "time_second contains too many digits"};

parse_timestamp_rfc3339([$+|Cs], _N, A, R, time_secfrac) ->
    parse_timestamp_rfc3339(Cs, 0, [], [{time_secfrac, lists:reverse(A)}|R], time_numoffset_hour);
parse_timestamp_rfc3339([$-|Cs], _N, A, R, time_secfrac) ->
    parse_timestamp_rfc3339(Cs, 0, [], [{time_secfrac, lists:reverse(A)}|R], time_numoffset_hour);
parse_timestamp_rfc3339([$Z|_Cs], _N, A, R, time_secfrac) ->
    lists:reverse([{time_secfrac, lists:reverse(A)}|R]);
parse_timestamp_rfc3339([C|Cs], N, A, R, time_secfrac) ->
    parse_timestamp_rfc3339(Cs, N+1, [C|A], R, time_secfrac);

parse_timestamp_rfc3339([$:|Cs], 2, A, R, time_numoffset_hour) ->
    parse_timestamp_rfc3339(Cs, 0, [], [{time_numoffset_hour, lists:reverse(A)}|R], time_numoffset_minute);
parse_timestamp_rfc3339([C|Cs], N, A, R, time_numoffset_hour) when N < 2  ->
    parse_timestamp_rfc3339(Cs, N+1, [C|A], R, time_numoffset_hour);
parse_timestamp_rfc3339([_C|_Cs], N, _A, _R, time_numoffset_hour) when N >= 2 ->
    {parse_error, "time_numoffset_hour contains too many digits"};

parse_timestamp_rfc3339([], 2, _A, R, time_numoffset_minute) ->
    lists:reverse(R);
parse_timestamp_rfc3339([$Z|_Cs], 2, _A, R, time_numoffset_minute) ->
    lists:reverse(R);
parse_timestamp_rfc3339([C|Cs], N, A, R, time_numoffset_minute) when N < 2  ->
    parse_timestamp_rfc3339(Cs, N+1, [C|A], R, time_numoffset_minute);
parse_timestamp_rfc3339([_C|_Cs], N, _A, _R, time_numoffset_minute) when N >= 2 ->
    {parse_error, "time_numoffset_minute contains too many digits"};

parse_timestamp_rfc3339([$:|Cs], 2, A, R, time_offset_hour) ->
    parse_timestamp_rfc3339(Cs, 0, [], [{time_offset_hour, lists:reverse(A)}|R], time_offset_minute);
parse_timestamp_rfc3339([C|Cs], N, A, R, time_offset_hour) when N < 2 ->
    parse_timestamp_rfc3339(Cs, N+1, [C|A], R, time_offset_hour);
parse_timestamp_rfc3339([_C|_Cs], N, _A, _R, time_offset_hour) when N >= 2 ->
    {parse_error, "time_offset_hour contains too many digits"};

parse_timestamp_rfc3339([], 2, _A, R, time_offset_minute) ->
    lists:reverse(R);
parse_timestamp_rfc3339([C|Cs], N, A, R, time_offset_minute) when N < 2  ->
    parse_timestamp_rfc3339(Cs, N+1, [C|A], R, time_offset_minute);
parse_timestamp_rfc3339([_C|_Cs], N, _A, _R, time_offset_minute) when N >= 2 ->
    {parse_error, "time_offset_minute contains too many digits"}.

