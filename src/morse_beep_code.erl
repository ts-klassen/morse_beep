-module(morse_beep_code).

-export([
        english/0
    ]).

-spec english(unicode:unicode_binary()) -> [ $. | $- ].
english() ->
    #{
        <<"a">> => ".-",
        <<"b">> => "-...",
        <<"c">> => "-.-.",
    }.

