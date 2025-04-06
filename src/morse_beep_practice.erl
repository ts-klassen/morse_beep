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
        <<"b\n">>,
        <<"c\n">>,
        <<"d\n">>,
        <<"e\n">>,
        <<"f\n">>,
        <<"g\n">>,
        <<"h\n">>,
        <<"i\n">>,
        <<"j\n">>,
        <<"k\n">>,
        <<"l\n">>,
        <<"m\n">>,
        <<"n\n">>,
        <<"o\n">>,
        <<"p\n">>,
        <<"q\n">>,
        <<"r\n">>,
        <<"s\n">>,
        <<"t\n">>,
        <<"u\n">>,
        <<"v\n">>,
        <<"w\n">>,
        <<"x\n">>,
        <<"y\n">>,
        <<"z\n">>,
        <<"0\n">>,
        <<"1\n">>,
        <<"2\n">>,
        <<"3\n">>,
        <<"4\n">>,
        <<"5\n">>,
        <<"6\n">>,
        <<"7\n">>,
        <<"8\n">>,
        <<"9\n">>
    ];
question(word) ->
    [
        <<"sos\n">>,
        <<"hello\n">>,
        <<"world\n">>,
        <<"morse\n">>,
        <<"code\n">>,
        <<"signal\n">>,
        <<"beep\n">>,
        <<"error\n">>,
        <<"urgent\n">>,
        <<"response\n">>,
        <<"transmit\n">>,
        <<"frequency\n">>,
        <<"receiver\n">>,
        <<"transmitter\n">>,
        <<"communication\n">>,
        <<"emergency\n">>,
        <<"timing\n">>,
        <<"pattern\n">>,
        <<"dah\n">>,
        <<"dit\n">>,
        <<"space\n">>,
        <<"repeat\n">>,
        <<"attention\n">>,
        <<"message\n">>,
        <<"letter\n">>,
        <<"word\n">>,
        <<"numeric\n">>,
        <<"character\n">>,
        <<"decode\n">>,
        <<"encode\n">>,
        <<"speed\n">>,
        <<"accuracy\n">>,
        <<"operator\n">>,
        <<"clarity\n">>,
        <<"tone\n">>,
        <<"array\n">>,
        <<"broadcast\n">>,
        <<"listen\n">>,
        <<"switch\n">>,
        <<"feedback\n">>,
        <<"network\n">>,
        <<"control\n">>,
        <<"protocol\n">>,
        <<"manual\n">>,
        <<"automatic\n">>,
        <<"command\n">>,
        <<"instruction\n">>,
        <<"channel\n">>,
        <<"transmission\n">>,
        <<"reception\n">>,
        <<"adjust\n">>,
        <<"amplify\n">>,
        <<"modulate\n">>,
        <<"demodulate\n">>,
        <<"oscillator\n">>,
        <<"battery\n">>,
        <<"current\n">>,
        <<"voltage\n">>,
        <<"resistor\n">>,
        <<"capacitor\n">>,
        <<"inductor\n">>,
        <<"circuit\n">>,
        <<"device\n">>,
        <<"module\n">>,
        <<"system\n">>,
        <<"source\n">>,
        <<"destination\n">>,
        <<"wavelength\n">>,
        <<"antenna\n">>,
        <<"efficiency\n">>,
        <<"static\n">>,
        <<"dynamic\n">>,
        <<"interface\n">>,
        <<"monitor\n">>,
        <<"digit\n">>,
        <<"dash\n">>,
        <<"dot\n">>,
        <<"break\n">>,
        <<"temporary\n">>,
        <<"clear\n">>,
        <<"test\n">>,
        <<"verify\n">>,
        <<"learn\n">>,
        <<"skill\n">>,
        <<"mastery\n">>,
        <<"adjustment\n">>,
        <<"binary\n">>,
        <<"signalize\n">>,
        <<"channeling\n">>,
        <<"encryption\n">>,
        <<"decryption\n">>,
        <<"synchrony\n">>,
        <<"protocols\n">>,
        <<"operational\n">>,
        <<"transceiver\n">>,
        <<"regulate\n">>,
        <<"amplitude\n">>,
        <<"continuity\n">>,
        <<"variation\n">>,
        <<"stability\n">>
    ];
question(sentence) ->
    [
        <<"the quick brown fox jumps over the lazy dog\n">>,
        <<"morse code is a beautiful method of communication\n">>,
        <<"erlang programming provides fault tolerance\n">>,
        <<"practice makes perfect in digital communication\n">>,
        <<"listening carefully improves code comprehension\n">>,
        <<"the operator adjusts the frequency with precision\n">>,
        <<"errors in transmission can be frustrating\n">>,
        <<"always verify your code for consistency\n">>,
        <<"communication signals require clear patterns\n">>,
        <<"sound beeps indicate success or failure\n">>,
        <<"the message was sent in a secure channel\n">>,
        <<"timely responses are critical in emergencies\n">>,
        <<"learning morse code can be both fun and challenging\n">>,
        <<"a well tuned system outperforms outdated hardware\n">>,
        <<"transmit the signal with confidence and clarity\n">>,
        <<"the receiver picks up every detailed tone\n">>,
        <<"accurate decoding leads to effective communication\n">>,
        <<"each element must be adjusted for optimum performance\n">>,
        <<"advanced encryption protects sensitive information\n">>,
        <<"a simple beep can convey complex messages\n">>,
        <<"timely adjustments enhance communication quality\n">>,
        <<"clear instructions lead to efficient operations\n">>,
        <<"synchronization in transmissions prevents delays\n">>,
        <<"high frequency signals travel faster than expected\n">>,
        <<"robust protocols ensure reliable connections\n">>,
        <<"the operator listens to every subtle error\n">>,
        <<"real time feedback is essential in control systems\n">>,
        <<"consistent practice builds communication skills\n">>,
        <<"every puzzle has a solution in its pattern\n">>,
        <<"the system maintains stability amidst turbulence\n">>,
        <<"digital signals create endless possibilities\n">>,
        <<"morse code has a rich history in communication\n">>,
        <<"effective modulation boosts signal strength\n">>,
        <<"new protocols are developed every day\n">>,
        <<"there is a perfect balance between speed and accuracy\n">>,
        <<"the device operates on a strict timing schedule\n">>,
        <<"efficient algorithms improve reception quality\n">>,
        <<"careful tuning minimizes transmission errors\n">>,
        <<"the beep indicates that the test was successful\n">>,
        <<"regular updates keep the network running smoothly\n">>,
        <<"familiarity with patterns enhances learning\n">>,
        <<"practice helps overcome challenges in coding\n">>,
        <<"even a small error can disrupt communication\n">>,
        <<"the transmitter sends crisp and clear signals\n">>,
        <<"reliable devices are the backbone of networks\n">>,
        <<"every call for help is answered promptly\n">>,
        <<"digital and analog modes offer diverse experiences\n">>,
        <<"operator expertise ensures timely responses\n">>,
        <<"the channel remained open for continuous communication\n">>,
        <<"quick adjustments lead to efficient outcomes\n">>,
        <<"lessons in morse code open a window to history\n">>,
        <<"robust systems require constant maintenance\n">>,
        <<"the sender repeats the message for added clarity\n">>,
        <<"every signal depends on precise modulation techniques\n">>,
        <<"long distance communication depends on system stability\n">>,
        <<"the operator monitors every transmission carefully\n">>,
        <<"our network adapts to challenging conditions\n">>,
        <<"decoding signals is both an art and a science\n">>,
        <<"complex problems can be solved with teamwork and persistence\n">>,
        <<"innovative technology drives progress in communication methods\n">>,
        <<"rapid signals require precise timing adjustments\n">>,
        <<"a clear pattern is a sign of effective transmission\n">>,
        <<"a well calibrated system prevents communication confusion\n">>,
        <<"transmission delays can impact overall performance\n">>,
        <<"accuracy in decoding is key to understanding messages\n">>,
        <<"system stability is essential for continuous operations\n">>,
        <<"the beep serves as a confirmation sound during tests\n">>,
        <<"quality channels improve overall signal integrity\n">>,
        <<"robust mechanisms protect the network against interruptions\n">>,
        <<"every beep carries vital information for operators\n">>,
        <<"listening carefully can reveal hidden transmission patterns\n">>,
        <<"consistent practice refines morse code skills remarkably\n">>,
        <<"technology continuously evolves with innovative methods\n">>,
        <<"the art of coding is marked by persistence and creativity\n">>,
        <<"digital decoding requires both logic and imagination\n">>,
        <<"testing different communication modes improves adaptability\n">>,
        <<"the operator adjusts controls with swift precision\n">>,
        <<"complex codes become simple with dedicated practice\n">>,
        <<"accuracy and timing are critical for successful transmissions\n">>,
        <<"learning from mistakes is a vital part of growth\n">>,
        <<"effective communication brings people together across distances\n">>,
        <<"signals travel through an intricate network of pathways\n">>,
        <<"integrity of the system depends on regular updates\n">>,
        <<"every element in the code serves a unique purpose\n">>,
        <<"smooth operations demand rigorous testing and maintenance\n">>,
        <<"continuous improvement drives technological advances\n">>,
        <<"reliable channels bridge vast geographical distances\n">>,
        <<"attention to detail prevents costly transmission errors\n">>,
        <<"collaboration among team members fosters innovative solutions\n">>,
        <<"the rhythm of morse code is uniquely captivating and historic\n">>,
        <<"technological mastery blends art seamlessly with science\n">>,
        <<"precision in every beep sharpens overall communication\n">>,
        <<"the operator’s expertise transforms chaos into organized order\n">>,
        <<"simple patterns can encode surprisingly complex messages\n">>,
        <<"signal patterns form the language of effective transmissions\n">>,
        <<"steady practice yields remarkable improvements in skill\n">>,
        <<"innovative thinking enhances overall system performance\n">>,
        <<"every challenge is an opportunity to learn and grow\n">>,
        <<"mastery of coding leads to seamless and efficient communication\n">>,
        <<"the future belongs to those who dare to innovate boldly\n">>
    ].
