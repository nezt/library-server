%%%-------------------------------------------------------------------
%%% @author Nestor <nest@nest-HP-530-Notebook-PC-GU339AA-ABM>
%%% @copyright (C) 2012, Nestor
%%% @doc
%%%
%%% @end
%%% Created : 13 Nov 2012 by Nestor <nest@nest-HP-530-Notebook-PC-GU339AA-ABM>
%%%-------------------------------------------------------------------
-module(library_server).

-behaviour(gen_server).

%% API
-export([start/0,start_link/0,insert_book/2,get_books_all/0, get_all/1,rent/2,
         get_books_available/0,get_books_unavailable/0,return_book/1,return_info/0]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {transactions,books}).


%%%===================================================================
%%% API
%%%===================================================================


%% start server
start() -> 
			gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
%% insert new book in database
insert_book(Id,NameBook)->
			gen_server:call(?MODULE, {insert,library,[{id,Id},{namebook,NameBook},{client,"null"},{status,"available"}]}).
%% Get all the books info
get_all(Table)->
			gen_server:call(?MODULE, {getall,Table}).
%% Get all the books info in a list format
get_books_all()->
			gen_server:call(?MODULE, {get_books,library,all}).
%% Get availables books in a list format
get_books_available()->
			gen_server:call(?MODULE, {get_books,library,"available"}).
%% Get unavailables books in a list format
get_books_unavailable()->
			gen_server:call(?MODULE, {get_books,library,"unavailable"}).
%% rent a book 
rent(Who, NameBook) -> 
			gen_server:call(?MODULE, {rent, Who, NameBook}).
%% return a book 
return_book(NameBook) -> 
			gen_server:call(?MODULE, {return,NameBook}).
%% return a history rent books
return_info() -> 
			gen_server:call(?MODULE, {return_info}).



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
    library_server_mnesia:create_tables(),
    {ok, #state{transactions=0,books=[undefined]}}.

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
    R=library_server_mnesia:put(Table,Data),
    {reply, R, #state{transactions=Transactions+1}};


handle_call({getall, Table}, _From, #state{transactions=Transactions}) ->
    R=library_server_mnesia:get(Table,all, none,none),
    {reply, R, #state{transactions=Transactions+1}};

handle_call({get_books, Table, all}, _From, #state{transactions=Transactions}) ->
    ListBooks=library_server_mnesia:get(Table,all, none,none),
    {reply, format_books(ListBooks), #state{transactions=Transactions+1}};

handle_call({get_books, Table, Type}, _From, #state{transactions=Transactions}) ->
    ListBooks=library_server_mnesia:get(Table,all, none,none),
     List= [Row || Row ={library,_,_,_,Status}<-ListBooks, Status =:= Type],

    {reply, format_books({Type,List}), #state{transactions=Transactions+1}};

handle_call({rent, Who, NameBook}, _From, #state{transactions=Transactions,books=RCBook}) ->
    Values = library_server_mnesia:get(library,all, none,none),
	 Oper=[begin
	       	  case Status of
	       	  "available"    -> 
	       	                    ok=library_server_mnesia:update({replace, library, client, Who, id, Id}),
	       	                    ok=library_server_mnesia:update({replace, library, status, "unavailable", id, Id}),
	       	                    {ok,{Who,Book}};
	       	  "unavailable"  -> {unavailable,Client}
               	  end
	         end || {library,Id,Book,Client,Status}<-Values, Book =:= NameBook],

       Res= case length(Oper) of 0->{error,book_notfound}; _->Oper end, 

    {reply, Res, #state{transactions=Transactions+1,books=[NameBook|RCBook]}};

handle_call({return,NameBook}, _From, #state{transactions=Transactions}) ->
    Values = library_server_mnesia:get(library,all, none,none),
	 Oper=[begin
	       	  case Status of
	       	  "available"    -> {available,Book};
	       	                    
	       	  "unavailable"  -> ok=library_server_mnesia:update({replace, library, client, null, id, Id}),
	       	                    ok=library_server_mnesia:update({replace, library, status, "available", id, Id}),
	       	                    {ok,{available,Book}}
               	  end
	         end || {library,Id,Book,_,Status}<-Values, Book =:= NameBook],
       Res= case length(Oper) of 0->{error,book_notfound}; _->Oper end, 
    {reply, Res, #state{transactions=Transactions+1}};

handle_call({return_info}, _From, #state{transactions=Transactions,books=RCBook}) ->
    Res=  [{transactions,Transactions},{rent_books,RCBook}],
    {reply, Res , #state{transactions=Transactions,books=RCBook}}.

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
{not_found_books,[]};

format_books({"available", ListBooks})->
{available_books, [ [{id,Id},{book,Libro},{status,Status}] || {library,Id,Libro,_,Status}<-ListBooks]};

format_books({"unavailable", ListBooks})->
{unavailable, [ [{id,Id},{book,Libro},{client,Client},{status,Status}] || {library,Id,Libro,Client,Status}<-ListBooks]};

format_books(ListBooks)->
{books, [[{id,Id},{book,Libro},{status,Status}] || {library,Id,Libro,_,Status}<-ListBooks]}.












