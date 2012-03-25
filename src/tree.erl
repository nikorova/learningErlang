-module(tree).
-export([empty/0, insert/3, lookup/2]).

empty() -> {node, 'nil'}.

insert(Key, Val, {node, 'nil'}) ->
	{node, {Key, Value, {node, 'nil'}, {node, 'nil'}}};

insert(NewKey, NewVal, {node, {Key, Val, Smaller, Larger}}) when NewKey < Key ->
	{node, {Key, Val, insert(NewKey, NewVal, Smaller), Larger}};

insert(NeKey, NewVal, {node, {Key, Val, Smaller, Larger}}) when NewKey > Key ->
	{node, {Key, val, Smaller, insert(NewKey, NewVal, Larger)}};

insert(Key, Val, {node, {Key, _, Smaller, Larger}}) -> 
	{node, {Key, Val, Smaller, Larger}}.

	