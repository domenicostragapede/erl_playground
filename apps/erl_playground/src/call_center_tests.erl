%%%-------------------------------------------------------------------
%%% @author Domenico Stragapede
%%% @doc
%%% Test module for the challenge.
%%% Use it typing eunit:test(call_center). on console.
%%% @end
%%%-------------------------------------------------------------------
-module(call_center_tests).
-author("Domenico Stragapede").

-compile(export_all).
-include_lib("eunit/include/eunit.hrl").


case1_test_() ->
    {"Initialize a client connection and interact with server",
        [
            ?_assertMatch(ok, sockclient:connect()),
            ?_assertMatch(ok, sockclient:send_create_session("348......")),
            ?_assertMatch(ok, sockclient:send_message("4")),
            ?_assertMatch(ok, sockclient:send_message("1")),
            ?_assertMatch(ok, sockclient:send_message("2")),
            ?_assertMatch(ok, sockclient:send_message("3")),
            ?_assertMatch(ok, sockclient:send_message("Hello world!")),
            ?_assertMatch(ok, sockclient:send_message("Are you there?")),
            ?_assertMatch(ok, sockclient:send_message("What can you do for me?")),
            ?_assertMatch(ok, sockclient:disconnect())
        ]
    }.

case2_test_() ->
    {"Initialize a client connection and interact with server,
      sending more than three message to the operator",
        [
            ?_assertMatch(ok, sockclient:connect()),
            ?_assertMatch(ok, sockclient:send_create_session("348......")),
            ?_assertMatch(ok, sockclient:send_message("1")),
            ?_assertMatch(ok, sockclient:send_message("2")),
            ?_assertMatch(ok, sockclient:send_message("3")),
            ?_assertMatch(ok, sockclient:send_message("4")),
            ?_assertMatch(ok, sockclient:send_message("Hello world!")),
            ?_assertMatch(ok, sockclient:send_message("Are you there?")),
            ?_assertMatch(ok, sockclient:send_message("What can you do for me?")),
            ?_assertMatch(ok, sockclient:disconnect())
        ]
    }.
