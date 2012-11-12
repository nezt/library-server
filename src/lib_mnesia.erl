%% ===================================================================
%% Library APP
%% Copyright (c) 2012 All Rights Reserved.
%% Nestor Ocampo <anezt_oh@hotmail.com>
%% ===================================================================
-module(lib_mnesia).
-include("library_server.hrl").
-include_lib("stdlib/include/qlc.hrl").

-export([create_tables/0, put/2, get/4, update/1,
	 delete/2, get_field/1, multi_delete/1]).

%% Creation of tables
%% @spec create_tables() -> ok.
create_tables() ->
     mnesia:create_table(library, [{attributes,record_info(fields, library)}]).

%% Write a record in mnesia
%% @spec put(Table::list() | atom(), Keys::[{atom(),list()}]) -> term().
put(Table, Keys) when is_list(Table) ->
    lib_mnesia:put(list_to_atom(Table),Keys);
put(Table, Keys)                     ->
    case util:is_proplist(Keys) of
	true  -> 
	    Set = list_to_tuple(
		    [Table|[proplists:get_value(X, Keys) ||
			       X <- mnesia:table_info(Table, attributes)]]),
	    Trnsc = fun() ->
			    mnesia:write(Set)
		    end,
	    reply(Trnsc);
	false -> nok
    end.

%% Select a value in mnesia
%% @spec get(Table::list() | atom(), Field::atom(), MatchField::atom(), MatchValue::list() ) -> term().
get(Table, Field, MatchField, MatchValue) when is_list(Table) ->
    lib_mnesia:get(list_to_atom(Table), Field, MatchField, MatchValue);

get(Table, all, none, none)               ->
    Trnsc = fun() ->
		    Q = qlc:q([X || X <- mnesia:table(Table)]),
		    qlc:e(Q)
	    end,
    reply(Trnsc);
get(Table, Field, none, none)             ->
    Trnsc = fun() ->
		    Q = qlc:q([element(get_field({Table, Field}), X) ||
				 X <- mnesia:table(Table)]),
		    qlc:e(Q)
	    end,
    reply(Trnsc);
get(Table, all, MatchField, MatchValue) ->
    Trnsc = fun() ->		    
		   Q = qlc:q([X || X <- mnesia:table(Table),
				   element(get_field({Table, MatchField}), X) =:= MatchValue]),
		   qlc:e(Q)
	   end,
    reply(Trnsc);
get(Table, Field, MatchField, MatchValue) ->
    Trnsc = fun() ->		    
		   Q = qlc:q([element(get_field({Table, Field}), X) || 
				 X <- mnesia:table(Table),
				 element(get_field({Table, MatchField}), X) =:= MatchValue]),
		   qlc:e(Q)
	   end,
    reply(Trnsc).

%% Update a value, options are replace or append 
%% @spec update({Type::atom(), Table::list() | atom(),Field::atom(), NewValue::term(), MatchField::atom(), MatchValue::term()}) -> term().
update({Type, Table, Field, NewValue, MatchField, MatchValue}) when is_list(Table) ->
    case Type of
	replace -> update({replace, list_to_atom(Table),
			   Field, NewValue, MatchField, MatchValue});
	append  -> update({append, list_to_atom(Table),
			   Field, NewValue, MatchField, MatchValue})
    end;
update({replace, Table, Field, NewValue, MatchField, MatchValue})                  ->
    Trnsc = fun() ->
		    Q = qlc:q([X || X <- mnesia:table(Table), 
				    element(get_field({Table, MatchField}), X) =:= MatchValue]),
		    [Occ|_]=qlc:e(Q),		    
		    UpdValue = setelement(get_field({Table, Field}), Occ, NewValue),
		    mnesia:write(UpdValue)
	    end,
    reply(Trnsc);
update({append, Table, Field, NewValue, MatchField, MatchValue})                   ->
    Trnsc = fun() ->
		    Q = qlc:q([X || X <- mnesia:table(Table), 
				    element(get_field({Table, MatchField}), X) =:= MatchValue]),
		    [Occ|_]=qlc:e(Q),
		    OldValue = element(get_field({Table, Field}), Occ),
		    Value=util:get_concat(NewValue, OldValue),
		    UpdValue = setelement(get_field({Table, Field}), Occ, Value),
		    mnesia:write(UpdValue)
	    end,
    reply(Trnsc).

%% Delete a record
%% @spec delete(Table::list() | atom(), Key::list()) -> term().
delete(Table, Key) when is_list(Table) ->
    delete(list_to_atom(Table), Key);
delete(Table, Key) ->
    Trnsc = fun() ->
		    mnesia:delete({Table, Key})
	    end,
    reply(Trnsc).


%% Multi delete records
%% @spec multi_delete([{atom() | [any()],_}]) -> 'ok'.
multi_delete([])                  -> ok;
multi_delete([{Table, Key}|Rest]) ->
    ok = delete(Table, Key),
    multi_delete(Rest).
    
%% reply of a transaction executed
%% @spec reply(Trnsc::term()) -> any()
reply(Trnsc) ->
    try
	{atomic, State} = mnesia:transaction(Trnsc),
	State
    catch
	_:Reason when is_list(Reason) -> {error, "== list empty =="};
	_:Reason                      -> {error, Reason}
    end.
	    
%% Match value given field
%% @spec get_field(Table::atom(),MatchField::atom()) -> term().
get_field({Table, MatchField})  ->
    Fields = mnesia:table_info(Table, attributes),
    get_field(Fields, MatchField).

%% record has one more element, for that reason use 2
get_field([], _)                        -> 2; 
get_field([Field | Fields], MatchField) ->
    case Field of
	MatchField ->
	    0 + get_field([], MatchField);
	_          ->
	    1 + get_field(Fields, MatchField)
    end.
