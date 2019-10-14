%%%-------------------------------------------------------------------
%%% @author ShiLei <wolfgang@ShiLeis-MacBook-Pro.local>
%%% @copyright (C) 2019, ShiLei
%%% @doc
%%%
%%% @end
%%% Created : 13 Oct 2019 by ShiLei <wolfgang@ShiLeis-MacBook-Pro.local>
%%%-------------------------------------------------------------------
-module(prop_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("proper/include/proper.hrl").

-define(SERVER, test_lock_server).
-define(TEST_MODULE, lock).
-define(TEST_CODE, [1,2,3]).
%%--------------------------------------------------------------------
%% @spec suite() -> Info
%% Info = [tuple()]
%% @end
%%--------------------------------------------------------------------
suite() ->
    [{timetrap,{minutes,1}}].

%%--------------------------------------------------------------------
%% @spec init_per_suite(Config0) ->
%%     Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    ct_property_test:init_per_suite(Config).

%%--------------------------------------------------------------------
%% @spec end_per_suite(Config0) -> term() | {save_config,Config1}
%% Config0 = Config1 = [tuple()]
%% @end
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    ok.

%%--------------------------------------------------------------------
%% @spec init_per_group(GroupName, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_group(_GroupName, Config) ->
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_group(GroupName, Config0) ->
%%               term() | {save_config,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% @end
%%--------------------------------------------------------------------
end_per_group(_GroupName, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% @spec init_per_testcase(TestCase, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_testcase(_TestCase, Config) ->
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_testcase(TestCase, Config0) ->
%%               term() | {save_config,Config1} | {fail,Reason}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
end_per_testcase(_TestCase, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% @spec groups() -> [Group]
%% Group = {GroupName,Properties,GroupsAndTestCases}
%% GroupName = atom()
%% Properties = [parallel | sequence | Shuffle | {RepeatType,N}]
%% GroupsAndTestCases = [Group | {group,GroupName} | TestCase]
%% TestCase = atom()
%% Shuffle = shuffle | {shuffle,{integer(),integer(),integer()}}
%% RepeatType = repeat | repeat_until_all_ok | repeat_until_all_fail |
%%              repeat_until_any_ok | repeat_until_any_fail
%% N = integer() | forever
%% @end
%%--------------------------------------------------------------------
groups() ->
    [].

%%--------------------------------------------------------------------
%% @spec all() -> GroupsAndTestCases | {skip,Reason}
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%% TestCase = atom()
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
all() ->
    [lock_function_test_case].

%%--------------------------------------------------------------------
%% @spec TestCase() -> Info
%% Info = [tuple()]
%% @end
%%--------------------------------------------------------------------
lock_function_test_case() ->
    [].

%%--------------------------------------------------------------------
%% @spec TestCase(Config0) ->
%%               ok | exit() | {skip,Reason} | {comment,Comment} |
%%               {save_config,Config1} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% Comment = term()
%% @end
%%--------------------------------------------------------------------
lock_function_test_case(Config) ->
    ct_property_test:quickcheck(prop(), Config).

prop() ->
    ?FORALL(Cmds, commands(?MODULE),
            ?TRAPEXIT(
               begin
                   ?TEST_MODULE:start_link(?SERVER,
                                           #{code          => ?TEST_CODE,
                                             auto_lock_ms  => 100,
                                             reset_code_ms => 100}),
                   {History, State, Result} = run_commands(?MODULE, Cmds),
                   ?TEST_MODULE:stop(?SERVER),
                   ?WHENFAIL(ct:log("History: ~w~nState: ~w\nResult: ~w~n",
                                       [History,State,Result]),
                             aggregate(command_names(Cmds), Result =:= ok))
               end)).

initial_state() ->
    #{buttons => [],
      lock_state => locked
     }.

next_state(S, _V, {call, ?TEST_MODULE, input, [_, Button]}) ->
    #{buttons := Buttons,
      lock_state := PrevLockState
     } = S,
    IsCodeComplete = (length(Buttons) =:= length(?TEST_CODE)),
    NewButtons = case {IsCodeComplete, PrevLockState} of
                     {true, locked} ->
                         [Button];
                     {true, unlocked} ->
                         Buttons;
                     _ ->
                         [Button | Buttons]
                 end,
    case {lists:reverse(NewButtons), PrevLockState} of
        {?TEST_CODE, unlocked} ->
            S;
        {?TEST_CODE, locked}  ->
            S#{buttons    => NewButtons,
               lock_state => unlocked};
        _ ->
            S#{buttons => NewButtons}
    end;
next_state(S, _V, {call, ct, sleep, _}) ->
    #{lock_state := LockState} = S,
    case LockState of
        locked ->
            S#{buttons => []};
        unlocked ->
            S#{lock_state => locked,
               buttons => []}
    end.

precondition(_, _) ->
    true.

postcondition(S, {call, ?TEST_MODULE, input, [_, Button]}, Result) ->
    #{buttons := Buttons,
      lock_state := PrevLockState
     } = S,
    case {PrevLockState, Result} of
        {unlocked, {ignored, Button}} ->
            lists:reverse(Buttons) =:= ?TEST_CODE;
        {locked, {unlocked, ?TEST_CODE}} ->
            lists:reverse([Button | Buttons]) =:= ?TEST_CODE;
        {locked, {locked, [Button]}} ->
           length(Buttons) =:= length(?TEST_CODE) orelse Buttons =:= [];
        {locked, {locked, ServerButtons}} ->
            lists:reverse([Button | Buttons]) =:= ServerButtons;
        _ ->
            false
    end;
postcondition(_, _, _) ->
    true.

command(_S) ->
    frequency([{2, {call, ?TEST_MODULE, input, [?SERVER, 1]}},
               {2, {call, ?TEST_MODULE, input, [?SERVER, 2]}},
               {2, {call, ?TEST_MODULE, input, [?SERVER, 3]}},
               {1, {call, ct, sleep, [150]}}
              ]).
