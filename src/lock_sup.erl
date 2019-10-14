%%%-------------------------------------------------------------------
%% @doc lock top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(lock_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    LockDefaultConfig = #{code          => [1,2,3,4],
                          auto_lock_ms  => 5000,
                          reset_code_ms => 30000},

    SupFlags = #{strategy  => one_for_all,
                 intensity => 3,
                 period    => 5},
    ChildSpecs = [#{id      => lock_server,
                    start   => {lock, start_link, [lock, LockDefaultConfig]},
                    restart => permanent}],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
