%% ===================================================================
%% Library util
%% Copyright (c) 2012 All Rights Reserved.
%% Nestor Ocampo <anezt_oh@hotmail.com>
%% ===================================================================
-module(util).
-export([get_concat/2,is_proplist/1,get_values/2, add_kv_pairs/2,key_member/2,
         replace_kv_pairs/2]).

%% Check if the list is a proplist
%% @spec is_proplist(List::list()) -> true | false.
is_proplist([]) -> true;

is_proplist([{Key, _}|Rest]) when is_atom(Key) ->
    is_proplist(Rest);

is_proplist([{Key, _}|Rest]) when is_list(Key) ->
    is_proplist(Rest);

is_proplist(_) -> false.

%% get proplist's value
%% @spec get_values(Proplist::list(), Keys::list()) -> term().
get_values(Proplist, Keys) ->
    [begin
	 Value = proplists:get_value(Key, Proplist),
	 {Value}
     end || Key <- Keys].

%% add new key/value pairs to an existing proplists
%% @spec add_kv_pairs(NewProplist::list(), OldProplist::list()) -> term().
add_kv_pairs(NewProplist, OldProplist) ->
    NAvalues =
	[{Key, Val} || {Key, Val} <- NewProplist,
		       false ==
			   proplists:is_defined(Key, OldProplist)],
    OldProplist ++ NAvalues.

%% key is member on a proplist
%% @spec key_member(Key::term(),Proplist::list()) -> ok | nok.
key_member(Key, Proplist) ->
    Keys = proplists:get_keys(Proplist),
    case lists:member(Key, Keys) of
	true  -> ok;
	false -> nok
    end.

%% Replace values in a proplist
%% @spec replace_kv_pairs(List::list(), Replacemements::term()) -> term().
replace_kv_pairs([],_)                                -> [];
replace_kv_pairs([{Key, Value} | Rest], Replacements) ->
    case proplists:get_value(Key, Replacements) of
	undefined ->
	    [{Key, Value}|replace_kv_pairs(Rest,Replacements)];
	NewValue   ->
	    NewReplacements = proplists:delete(Key,Replacements),
	    [{Key, NewValue}|replace_kv_pairs(Rest,NewReplacements)]
    end.


%% don't know what type is, then evaluate all known types :)
%% @spec get_concat(OldValue::term(),NewValur::term()) -> term().
get_concat(OldValue, NewValue) when is_list(OldValue),
				    is_list(NewValue) ->
    OldValue ++ NewValue;
get_concat(OldValue, NewValue) when is_number(OldValue),
				    is_number(NewValue) ->
    OldValue + NewValue.


