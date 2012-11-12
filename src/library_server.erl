%%%-------------------------------------------------------------------
%%% @author Nestor <nest@nest-HP-530-Notebook-PC-GU339AA-ABM>
%%% @copyright (C) 2012, Nestor
%%% @doc
%%%
%%% @end
%%% Created :  6 Nov 2012 by Nestor <nest@nest-HP-530-Notebook-PC-GU339AA-ABM>
%%%-------------------------------------------------------------------
-module(library_server).

-behaviour(gen_server).

%% API
-export([start/0,start_link/0,insert_book/2,get_books_all/0, get_all/1,rent/2,
         get_books_available/0,get_books_unable/0,return_book/1]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {transactions}).


%%%===================================================================
%%% API
%%%===================================================================



start() -> 
			gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

insert_book(Id,NameBook)->
			gen_server:call(?MODULE, {insert,library,[{id,Id},{namebook,NameBook},{client,"null"},{status,"available"}]}).
                                               
get_all(Table)->
			gen_server:call(?MODULE, {getall,Table}).
get_books_all()->
			gen_server:call(?MODULE, {get_books,library,all}).
get_books_available()->
			gen_server:call(?MODULE, {get_books,library,"available"}).
get_books_unable()->
			gen_server:call(?MODULE, {get_books,library,"unable"}).
rent(Who, NameBook) -> 
			gen_server:call(?MODULE, {rent, Who, NameBook}).

return_book(NameBook) -> 
			gen_server:call(?MODULE, {return,NameBook}).


%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initiates the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    ok = mnesia:start(),
    lib_mnesia:create_tables(),
    {ok, #state{transactions=0}}.

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
handle_call({insert, Table, Data}, _From, #state{transactions=Transactions}) ->
    R=lib_mnesia:put(Table,Data),
    {reply, R, #state{transactions=Transactions+1}};


handle_call({getall, Table}, _From, #state{transactions=Transactions}) ->
    R=lib_mnesia:get(Table,all, none,none),
    {reply, R, #state{transactions=Transactions+1}};

%Type  all
handle_call({get_books, Table, all}, _From, #state{transactions=Transactions}) ->
    ListBooks=lib_mnesia:get(Table,all, none,none),
    {reply, format_books(ListBooks), #state{transactions=Transactions+1}};

%Type  available/unable
handle_call({get_books, Table, Type}, _From, #state{transactions=Transactions}) ->
    ListBooks=lib_mnesia:get(Table,all, none,none),
     List= [Row || Row ={library,_,_,_,Status}<-ListBooks, Status =:= Type],

    {reply, format_books({Type,List}), #state{transactions=Transactions+1}};


handle_call({rent, Who, NameBook}, _From, #state{transactions=Transactions}) ->
    Values = lib_mnesia:get(library,all, none,none),
	 Oper=[begin
	       	  case Status of
	       	  "available"    -> 
	       	                    ok=lib_mnesia:update({replace, library, client, Who, id, Id}),
	       	                    ok=lib_mnesia:update({replace, library, status, "unable", id, Id}),
	       	                    {ok,{Who,Book}};
	       	  "unable"  -> {unable,Client}
               	  end
	         end || {library,Id,Book,Client,Status}<-Values, Book =:= NameBook],

       Res= case length(Oper) of 0->{error,book_notfound}; _->Oper end, 
    {reply, Res, #state{transactions=Transactions+1}};


handle_call({return,NameBook}, _From, #state{transactions=Transactions}) ->
    Values = lib_mnesia:get(library,all, none,none),
	 Oper=[begin
	       	  case Status of
	       	  "available"    -> {available,Book};
	       	                    
	       	  "unable"  -> ok=lib_mnesia:update({replace, library, client, null, id, Id}),
	       	                    ok=lib_mnesia:update({replace, library, status, "available", id, Id}),
	       	                    {ok,{available,Book}}
               	  end
	         end || {library,Id,Book,_,Status}<-Values, Book =:= NameBook],
       Res= case length(Oper) of 0->{error,book_notfound}; _->Oper end, 
    {reply, Res, #state{transactions=Transactions+1}}.


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

format_books([])->
io:format("~n\t\t\t ***** Library ***** ~n~n \t\t\t There are not books ~n",[]),
ok;



format_books({"available", ListBooks})->
io:format("~n\t\t\t ***** Library ***** ~n~n 
               \t\t id \t\t title \t\t status ~n~n",[]),

[io:format("\t\t ~p \t\t ~p \t ~p ~n",[Id,Libro,Status])
 || {library,Id,Libro,_,Status}<-ListBooks],
ok;


format_books({"unable", ListBooks})->
io:format("~n\t\t\t ***** Library ***** ~n~n 
               \t\t id \t\t title \t\t client \t\t status ~n~n",[]),
[io:format("\t\t ~p \t\t ~p \t\t ~p \t ~p ~n",[Id,Libro,Client,Status])
 || {library,Id,Libro,Client,Status}<-ListBooks],
ok;



format_books(ListBooks)->
io:format("~n\t\t\t ***** Library ***** ~n~n 
               \t\t id \t\t title \t\t status ~n~n",[]),
[io:format("\t\t ~p \t\t ~p \t ~p ~n",[Id,Libro,Status])
 || {library,Id,Libro,_,Status}<-ListBooks],
ok.











