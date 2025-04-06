-module(morse_beep_practice).

-export([
        listen/2
    ]).

-export_type([
        mode/0
    ]).

-type mode() :: char | word | sentence.

-spec listen(mode(), morse_beep:options()) -> no_return().
listen(Mode, Opts) ->
    Question = question(Mode),
    morse_beep:await(Question, Opts),
    Answer = klsn_io:get_line(),
    case Question =:= Answer of
        true ->
            os:cmd("beep -f 4000 -l 100 -D 100 -n -f 3200 -l 300");
        false ->
            os:cmd("beep -f 320 -l 100 -D 100 -n -f 320 -l 300")
    end,
    klsn_io:format("~ts~n", [Question]),
    listen(Mode, Opts).

question(Mode) ->
    List = question_list(Mode),
    Index = rand:uniform(length(List)),
    lists:nth(Index, List).

question(char) ->
    [
        <<"a\n">>,
        <<"b\n">>
    ];
question(word) ->
    [
        <<"hello\n">>,
        <<"world\n">>,
        <<"programming\n">>,
        <<"Erlang\n">>
    ];
question(sentence) ->
    [
        <<"hello world\n">>,
        <<"morse code\n">>,
        <<"I love Erlang\n">>,
        <<"This is a test\n">>
    ].
