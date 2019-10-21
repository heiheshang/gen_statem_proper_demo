-module(lock).
-behaviour(gen_statem).

%% APIs
-export([start_link/1, start_link/2, stop/1,
         input/1, input/2]).
%% gen_statem callbacks
-export([callback_mode/0, init/1]).
%% StateName callbacks
-export([locked/3, unlocked/3]).

-record(data, {code_length, buttons, code, auto_lock_ms, reset_code_ms}).

-define(SERVER, ?MODULE).
-define(HANDLE_COMMON,
        ?FUNCTION_NAME(T,C,D) -> handle_common(T,C,D)).

handle_common(_, _, Data) ->
    {keep_state, Data}.

%% APIs
start_link(Config) ->
    start_link(?SERVER, Config).

start_link(Name, Config) ->
    gen_statem:start_link({local, Name}, ?MODULE, Config, []).

stop(Name) ->
    gen_statem:stop(Name).

input(Button) ->
    input(?SERVER, Button).

input(Name, Button) ->
    gen_statem:cast(Name, {input, Button}).

%% Callbacks
callback_mode() ->
    state_functions.

init(#{code          := Code,
       auto_lock_ms  := AutoLockMS,
       reset_code_ms := ResetCodeMS}) ->
    {ok, locked, #data{code          = Code,
                       code_length   = length(Code),
                       auto_lock_ms  = AutoLockMS,
                       reset_code_ms = ResetCodeMS,
                       buttons       = []}}.

locked(timeout, _, Data) ->
    {keep_state, Data#data{buttons=[]}};
locked(cast, {input, Button}, Data) ->
    #data{buttons = Buttons,
          code = Code,
          auto_lock_ms  = AutoLockMS,
          reset_code_ms = ResetCodeMS,
          code_length = CodeLength
         } = Data,
    NewButtons = if CodeLength =:= length(Buttons) ->
                         [Button];
                    true ->
                         [Button | Buttons]
                 end,
    NewData = Data#data{buttons = NewButtons},

    case lists:reverse(NewButtons) of
        Code ->
            {next_state, unlocked, NewData,
             [{state_timeout, AutoLockMS, locked}]};
        _ ->
            {keep_state, NewData, ResetCodeMS}
    end;
?HANDLE_COMMON.

unlocked(state_timeout, locked, Data) ->
    {next_state, locked, Data#data{buttons=[]}};
unlocked(cast, {input, _Button}, _Data) ->
    keep_state_and_data;
?HANDLE_COMMON.
