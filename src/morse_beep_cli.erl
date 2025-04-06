-module(morse_beep_cli).

-export([
        main/1
    ]).


main(["await" | Args]) ->
    application:ensure_all_started(morse_beep),
    await(Args);
main(Args) ->
    help(Args).


await(Args) ->
    Param = parse_args(Args, #{}),
    Opts = opts_from_param(Param),
    await_(Opts).

await_(Opts) ->
    morse_beep:await(klsn_io:get_line(), Opts),
    await_(Opts).


help(_) ->
    klsn_io:format("morse beep:
    await:
        --short-frequency: (integer)
        --short-length: (integer)
        --long-frequency: (integer)
        --long-length: (integer)
        --beep-delay: (integer)
        --char-delay: (integer)
        --word-delay: (integer)
        --sentence-delay: (integer)
        --atempo: (float)
    ").

parse_args([], Ack) ->
    Ack;
parse_args([[$-, $- | Key], Value | Tail], Ack) ->
    parse_args(Tail, klsn_map:upsert([iolist_to_binary(Key)], iolist_to_binary(Value), Ack)).

opts_from_param(Param) ->
    klsn_map:filter(#{
        short => {value, klsn_map:filter(#{
                frequency => maybe_to_integer(klsn_map:lookup([<<"short-frequency">>], Param)),
                length => maybe_to_integer(klsn_map:lookup([<<"short-length">>], Param))
            })},
        long => {value, klsn_map:filter(#{
                frequency => maybe_to_integer(klsn_map:lookup([<<"long-frequency">>], Param)),
                length => maybe_to_integer(klsn_map:lookup([<<"long-length">>], Param))
            })},
        beep_delay => maybe_to_integer(klsn_map:lookup([<<"beep-delay">>], Param)),
        char_delay => maybe_to_integer(klsn_map:lookup([<<"char-delay">>], Param)),
        word_delay => maybe_to_integer(klsn_map:lookup([<<"word-delay">>], Param)),
        sentence_delay => maybe_to_integer(klsn_map:lookup([<<"sentence-delay">>], Param)),
        atempo => maybe_to_float(klsn_map:lookup([<<"atempo">>], Param))
    }).

maybe_to_integer(none) ->
    none;
maybe_to_integer({value, Bin}) ->
    {value, binary_to_integer(Bin)}.

maybe_to_float(none) ->
    none;
maybe_to_float({value, Bin}) ->
    {value, try
        binary_to_integer(Bin)
    catch
        error:badarg ->
            binary_to_float(Bin)
    end}.

