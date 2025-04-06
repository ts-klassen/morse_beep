-module(morse_beep).

-export([
        await/2,
        main/1
    ]).

-export_type([
        options/0
    ]).

-type options() :: #{
        short => #{
                frequency => pos_integer(),
                length => pos_integer()
            },
        long => #{
                frequency => pos_integer(),
                length => pos_integer()
            },
        beep_delay => pos_integer(),
        char_delay => pos_integer(),
        word_delay => pos_integer(),
        sentence_delay => pos_integer(),
        time_scale => float()
    }.

main(Args) ->
    morse_beep_cli:main(Args).

-spec await(unicode:unicode_binary(), options()) -> ok.
await(Str, Opts) ->
    List = parse_string(<<Str/binary, "\n">>, morse_beep_code:english(), []),
    Args = build_beep_args(List, Opts, []),
    os:cmd(binary_to_list(<<"beep", Args/binary>>)),
    ok.
    

build_beep_args([], _Opts, Ack) ->
    <<" -n", Args/binary>> = iolist_to_binary(lists:reverse(Ack)),
    Args;
build_beep_args([short, word_separator | Tail], Opts, Ack) ->
    build_beep_args(Tail, Opts, [beep_args_iolist(short, word, Opts)|Ack]);
build_beep_args([long, word_separator | Tail], Opts, Ack) ->
    build_beep_args(Tail, Opts, [beep_args_iolist(long, word, Opts)|Ack]);
build_beep_args([short, sentence_separator | Tail], Opts, Ack) ->
    build_beep_args(Tail, Opts, [beep_args_iolist(short, sentence, Opts)|Ack]);
build_beep_args([long, sentence_separator | Tail], Opts, Ack) ->
    build_beep_args(Tail, Opts, [beep_args_iolist(long, sentence, Opts)|Ack]);
build_beep_args([short, char_separator | Tail], Opts, Ack) ->
    build_beep_args(Tail, Opts, [beep_args_iolist(short, char, Opts)|Ack]);
build_beep_args([long, char_separator | Tail], Opts, Ack) ->
    build_beep_args(Tail, Opts, [beep_args_iolist(long, char, Opts)|Ack]);
build_beep_args([short | Tail], Opts, Ack) ->
    build_beep_args(Tail, Opts, [beep_args_iolist(short, beep, Opts)|Ack]);
build_beep_args([long | Tail], Opts, Ack) ->
    build_beep_args(Tail, Opts, [beep_args_iolist(long, beep, Opts)|Ack]).

beep_args_iolist(ShortLong, Delay, Opts) ->
    [
        <<" -n -f ">>,
        beep_args(f, ShortLong, Opts),
        <<" -l ">>,
        beep_args(l, ShortLong, Opts),
        <<" -D ">>,
        beep_args('D', Delay, Opts)
    ].

beep_args(f, Type, Opts) ->
    F = klsn_map:get([Type, frequency], Opts, 2000),
    integer_to_binary(round(F));
beep_args(Mode, Key, Opts) ->
    Float = beep_args_(Mode, Key, Opts) * maps:get(time_scale, Opts, 1),
    integer_to_binary(round(Float)).

beep_args_(l, short, Opts) ->
    klsn_map:get([short, length], Opts, 50);
beep_args_(l, long, Opts) ->
    klsn_map:get([long, length], Opts, 100);
beep_args_('D', beep, Opts) ->
    maps:get(beep_delay, Opts, 50);
beep_args_('D', char, Opts) ->
    maps:get(char_delay, Opts, 100);
beep_args_('D', word, Opts) ->
    maps:get(word_delay, Opts, 200);
beep_args_('D', sentence, Opts) ->
    maps:get(sentence_delay, Opts, 400).
    


parse_string(<<>>, _, Ack) ->
    lists:flatten(lists:reverse(Ack));
parse_string(<<Char/integer, Tail/binary>>, English, Ack) ->
    Str = <<case Char of
        C when $A =< C, C =< $Z ->
            C + $a - $A;
        C ->
            C
    end/integer>>,
    case {maps:find(Str, English), Char, Ack} of
        {{ok, Code}, _, _} ->
            Args = lists:map(fun
                ($.)-> short;
                ($-)-> long
            end, Code),
            parse_string(Tail, English, [char_separator, Args|Ack]);
        {_, $\s, [word_separator|_]} ->
            parse_string(Tail, English, Ack);
        {_, $\s, [sentence_separator|_]} ->
            parse_string(Tail, English, Ack);
        {_, $\s, [char_separator|T]} ->
            parse_string(Tail, English, [word_separator|T]);
        {_, _, [sentence_separator|_]} ->
            parse_string(Tail, English, Ack);
        {_, _, [char_separator|T]} ->
            parse_string(Tail, English, [sentence_separator|T]);
        {_, _, [word_separator|T]} ->
            parse_string(Tail, English, [sentence_separator|T]);
        {_, _, _} ->
            parse_string(Tail, English, [sentence_separator|Ack])
    end.

