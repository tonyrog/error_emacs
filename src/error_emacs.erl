%%%---- BEGIN COPYRIGHT --------------------------------------------------------
%%%
%%% Copyright (C) 2007 - 2012, Rogvall Invest AB, <tony@rogvall.se>
%%%
%%% This software is licensed as described in the file COPYRIGHT, which
%%% you should have received as part of this distribution. The terms
%%% are also available at http://www.rogvall.se/docs/copyright.txt.
%%%
%%% You may opt to use, copy, modify, merge, publish, distribute and/or sell
%%% copies of the Software, and permit persons to whom the Software is
%%% furnished to do so, under the terms of the COPYRIGHT file.
%%%
%%% This software is distributed on an "AS IS" basis, WITHOUT WARRANTY OF ANY
%%% KIND, either express or implied.
%%%
%%%---- END COPYRIGHT ----------------------------------------------------------
%%% @author Tony Rogvall <tony@rogvall.se>
%%% @doc
%%%    See README.md
%%% @end
%%% Created : 26 May 2012 by Tony Rogvall <tony@rogvall.se>
%%%-------------------------------------------------------------------
-module(error_emacs).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([start/0, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, 
	{
	  timer,   %% retry (to find eval timer)
	  pid      %% eval pid
	}).

%%-define(dbg(F,A), io:format("~s:~w: debug: "++(F)++"\n",[?MODULE,?LINE|(A)])).
-define(dbg(F,A), ok).
%% -define(info(F,A), io:format("~s:~w: info: "++(F)++"\n",[?MODULE,?LINE|(A)])).
-define(info(F,A), ok).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start() ->
    gen_server:start({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:call(?SERVER, stop).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    ?info("init", []),
    %% We link to shell process to locate errors 
    process_flag(trap_exit, true),
    State = link_eval(#state{}),
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Request, _From, State) ->
    {reply, {error, bad_call}, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

handle_info(_Exit={'EXIT',Pid,Reason}, State) when Pid =:= State#state.pid ->
    ?dbg("error_emacs: detected crash ~p", [_Exit]),
    handle_exit(Reason),
    %% short wait to allow new eval to be started
    Tmr = erlang:start_timer(100, self(), find_shell_eval),
    %% link_eval(State),
    State1 = State#state { pid=undefined, timer = Tmr },
    {noreply, State1};
handle_info({timeout,Tmr,find_shell_eval}, State) 
  when State#state.timer =:= Tmr ->
    %% garbage_collect here to avoid that data from 'EXIT' Reason etc
    %% get stuck for a longer time. Like magic binaries used by 
    %% resource notifications etc.
    garbage_collect(),
    ?dbg("error_emacs: retry to find shell eval proc", []),
    State1 = link_eval(State),
    {noreply, State1};
handle_info(crash, State) ->
    erlang:error(what_a_hell),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

handle_exit({_Reason, Trace}) ->
    ?info("crash: ~p trace=~p", [_Reason,Trace]),
    case Trace of
	[] -> ok;
	_ -> find_error(Trace)
    end;
handle_exit(_Reason) ->
    ?info("no match: ~p", [_Reason]),
    ok.

%% locate error is "user" code - skip standard libraries etc
find_error([{M,F,_A,Loc}|Tail]) when is_atom(M), is_atom(F), is_list(Loc) ->
    case code:which(M) of
	non_existing ->
	    %% probably undefined function call, find caller
	    find_error(Tail);
	preloaded ->
	    find_error(Tail);	    
	Path ->
	    case lists:prefix(code:lib_dir(), Path) of
		true ->
		    find_error(Tail);
		false ->
		    handle_location(Path,Loc)
	    end
    end;
find_error([{M,F,_A}|Tail]) when is_atom(M), is_atom(F) ->
    case code:which(M) of
	non_existing ->
	    ok;
	Path ->
	    case lists:prefix(code:lib_dir(), Path) of
		true ->
		    find_error(Tail);
		false ->
		    ok
	    end
    end;
find_error(_) ->
    ok.

handle_location(Path,Loc) ->
    case {proplists:get_value(file,Loc),
	  proplists:get_value(line,Loc)} of
	{undefined, _} -> ok;
	{_, undefined} -> ok;
	{File,Line} ->
	    handle_file(Path, File, Line)
    end.

handle_file(BeamPath, File, Line) ->
    Dir = filename:dirname(BeamPath),
    case filename:basename(Dir) of
	"ebin" ->
	    Dir1 = filename:dirname(Dir),
	    Source = filename:join([Dir1,"src",File]),
	    case filelib:is_regular(Source) of
		true ->
		    launch_emacs(Source, Line);
		false ->
		    Source1 = filename:join([Dir1,File]),
		    case filelib:is_regular(Source1) of
			true ->
			    launch_emacs(Source1, Line);
			false ->
			    ok
		    end
	    end;
	_ ->
	    Source = filename:join([Dir,File]),
	    case filelib:is_regular(Source) of
		true ->
		    launch_emacs(Source, Line);
		false ->
		    ok
	    end
    end.

%%
%% execute: emacsclient -n -e '(find-file "<file>")' '(goto-line <line>)'
%%             '(end-of-line)' '(set-mark (line-beginning-position))'
%%
launch_emacs(File, Line) ->
    EmacsClient = "emacsclient",
    ?dbg("file=~p, line=~p", [File, Line]),
    As = ["-n",
	  "-e", 
	  "'(find-file \""++File++"\")'",
	  "'(goto-line " ++ integer_to_list(Line) ++ ")'",
	  "'(end-of-line)'",
	  "'(set-mark (line-beginning-position))'"],
    Args = lists:flatten(lists:map(fun(A) -> [" ",A] end, As)),
    Command = EmacsClient ++ Args,
    ?dbg("execute: [~s]\n", [Command]),
    _Res = os:cmd(Command),
    ?dbg("result = [~s]\n", [_Res]),
    ok.

link_eval(State) ->
    case shell:whereis_evaluator() of
	undefined ->
	    Tmr = erlang:start_timer(1000, self(), find_shell_eval),
	    State#state { timer = Tmr, pid = undefined };
	EvalPid ->
	    ?dbg("link to ~p", [EvalPid]),
	    link(EvalPid),
	    State#state{ timer = undefined, pid = EvalPid }
    end.
