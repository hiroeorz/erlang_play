%%%-------------------------------------------------------------------
%%% File    : ka_chan.erl
%%% Author  : HIROE Shin <shin@ceres.komatsuelec.co.jp>
%%% Description : 
%%%
%%% Created : 11 Sep 2009 by HIROE Shin <shin@ceres.komatsuelec.co.jp>
%%%-------------------------------------------------------------------
-module(ka_chan).

-behaviour(supervisor).

%% API
-export([start_link/0, start_in_shell/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the supervisor
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_in_shell() ->
    {ok, Pid} = supervisor:start_link({local, ?SERVER}, 
				      ?MODULE, []),
    unlink(Pid).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Func: init(Args) -> {ok,  {SupFlags,  [ChildSpec]}} |
%%                     ignore                          |
%%                     {error, Reason}
%% Description: Whenever a supervisor is started using 
%% supervisor:start_link/[2,3], this function is called by the new process 
%% to find out about restart strategy, maximum restart frequency and child 
%% specifications.
%%--------------------------------------------------------------------
init([]) ->

    ICHIRO = {ic,
	     {ichiro, start_link,[]},
	     permanent,
	     2000,
	     worker,
	     [ichiro]},

    JIRO = {ji,
	    {jiro, start_link,[]},
	    permanent,
	    2000,
	    worker,
	    [jiro]},

    SABURO = {sa,
	     {saburo, start_link,[]},
	     permanent,
	     2000,
	     worker,
	     [saburo]},

    {ok,{{one_for_all,3,10}, [ICHIRO, JIRO, SABURO]}}.

%%====================================================================
%% Internal functions
%%====================================================================
