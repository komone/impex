%%
%% Tuple Importer/Exporter
%% Copyright (c) 2009 Steve Davis <steven.charles.davis@gmail.com>. All rights reserved.
%% LICENSE: Creative Commons Non-Commercial License V 3.0 
%% http://creativecommons.org/licenses/by-nc/3.0/us/
%%
-module(impex).
-vsn("0.1").
-author('steve@simulacity.com').

-include("../include/impex.hrl").

-export([new/1, run/2, regex_info/0, regex/1]). 
-export([log/1, log/3, stats/0, stats/1]).

%% extremely temporary - fix paths and resources for impex
new(Format) when is_atom(Format) ->
	new(filename:join([code:lib_dir(?MODULE), "priv/lib", 
		atom_to_list(Format) ++ ".form"]));
		
new(Format) ->
	{ok, Form=#form{}} = file:script(Format),
	Terminals = find_terminals(Form#form.defs, []),
	log(full, "Terminals: ~p~n", [Terminals]),
	Tokenizer = create_tokenizer(Terminals, [], []),
	log(full, "Optimizing patterns...~n~p~n", [Form#form.defs]), 
	Defs = find_lists(Form#form.defs, Terminals),
	Defs1 = normalize(Defs),
	Defs2 = optimize(Defs1),
	log(full, "~p~n", [Defs2]), 
	impex_parser:new(Form#form{defs = Defs2}, Tokenizer).

%%
run(Source, Format) ->
	Parser = new(Format),
	Parser:import_file(Source).

% 
regex_info() ->
	Defs = [io:format("  ~p = ~p~n", [X, regex(X)]) 
		|| X <- [alpha, alphanum, digit, digits, hex, number, integer, string]],
	{ok, length(Defs)}.
	
% Some predefined regular expressions 
regex(alpha)    -> "[A-Za-z]+";
regex(alphanum) -> "[A-Za-z0-9]+";
regex(digits)   -> "[0-9]+";
regex(hex)      -> "#?[0-9A-fa-f]+";
regex(float)    -> "[+-]?([0-9]+\.)?[0-9]+([Ee][+-]?[0-9]+)?";
regex(ident)    -> "(?:[A-Za-z_][A-Za-z0-9_]*)";
%regex(number)   -> "[+-]?[0-9]+[\.[0-9]+[[Ee][+-]?[0-9]+]?"; % TODO STILL ALLOWS 11.e1
regex(number)   -> "(?:[+-]?[0-9]+(?:[\.][0-9]+(?:[Ee][+-]?[0-9]+)?)?)";
regex(integer)  -> "[+-]?[0-9]+";
regex(string)   -> "\"[^\"]*\"";
regex(Case)     -> Case.

%% TODO: Either remove or change later?
%% full, normal, terse, none
log(none) -> erase(impex_log_level), ok;
log(Level) when is_atom(Level) -> 
	put(impex_log_level, Level), ok.
log(Level, Format, Args) ->
	case Level >= get(impex_log_level) of
	true -> io:format(Format, Args);
	_ -> ok
	end.

%%
stats() ->
	get(impex_stats).
stats(clear) ->
	erase(impex_stats);
stats(match) ->
	case get(impex_stats) of
	undefined -> put(impex_stats, {1, 0});
	{X, Y} -> put(impex_stats, {X + 1, Y})
	end;
stats(nomatch) ->
	case get(impex_stats) of
	undefined -> put(impex_stats, {0, 1});
	{X, Y} -> put(impex_stats, {X, Y + 1})
	end.

%%
%% Internal
%%

%
create_tokenizer([{regex, Spec}|T], Acc, Acc1) ->
	create_tokenizer(T, ["|", regex(Spec)|Acc], Acc1);
create_tokenizer([H|T], Acc, Acc1) when is_list(H), length(H) > 1 ->
	create_tokenizer(T, ["|", H|Acc], Acc1);
create_tokenizer([H|T], Acc, Acc1) ->
	create_tokenizer(T, Acc, [escape(H)|Acc1]);
create_tokenizer(T = [], ["|"|Acc], Acc1 = []) -> % strip trailing |
	create_tokenizer(T, Acc, Acc1);
create_tokenizer([], Acc, Acc1) ->
	lists:flatten(["(", lists:reverse(Acc), 
		"[", lists:reverse(Acc1), "])"]).


% 
find_lists(Defs, Terminals) ->
	[find_lists(Type, Terminals, Patterns, []) || {Type, Patterns} <- Defs].
	%?FinalDefs = [replace_lists(Type, Patterns) || {Type, Patterns} <- Defs].
% 
find_lists(Type, Terminals, [H = {Value, Separator, Type}|T], Acc) when is_atom(Value) ->
	case lists:member(Separator, Terminals) of 
	true ->	
		{Type, [{list, Separator, Value}]}; % NOTE: this will ignore any other pattern
	false -> 
		find_lists(Type, Terminals, T, [H|Acc])
	end;
find_lists(Type, Terminals, [H|T], Acc) ->
	find_lists(Type, Terminals, T, [H|Acc]);
find_lists(Type, _, [], Acc) ->
	{Type, lists:reverse(Acc)}.

%
normalize(Defs) ->
	[{Type, normalize_patterns(Patterns, [])} || {Type, Patterns} <- Defs].
%
normalize_patterns([H|T], Acc) ->
	Pattern = 
		case H of
		_ when is_atom(H) -> [H];
		{regex, _} -> [H];
		{list, _, _} -> [H];
		{Value} when is_list(Value) -> [H]; % keywords
		% otherwise, make the pattern tuple into a list
		_ when is_tuple(H) -> tuple_to_list(H); 
		_ -> H
		end,
	normalize_patterns(T, [Pattern|Acc]);
normalize_patterns([], Acc) ->
	lists:reverse(Acc).
%
optimize(Defs) ->
	% for now, a simple sort
	[{Type, lists:sort(Patterns)} || {Type, Patterns} <- Defs].
	
%%
find_terminals([{Type, Cases}|Rest], Acc) when is_atom(Type) ->
	Terminals = terminals(Cases, []),
	find_terminals(Rest, lists:append(Terminals, Acc));
find_terminals([], Acc) ->
	make_set(Acc, []).
%	
terminals([Case|Rest], Acc) when is_atom(Case); is_integer(Case) ->
	terminals(Rest, Acc);
terminals([Case|Rest], Acc) when is_list(Case) ->
	terminals(Rest, [Case|Acc]);
terminals([Case = {regex, _}|Rest], Acc) ->
	terminals(Rest, [Case|Acc]);
terminals([Case|Rest], Acc) when is_tuple(Case) ->
	T = [X || X <- tuple_to_list(Case), is_list(X)],
	terminals(Rest, lists:append(T, Acc));
terminals([], Acc) ->
	lists:reverse(Acc).

%%
make_set([H|T], Acc) ->
	case lists:member(H, T) of
	true -> make_set(T, Acc);
	false -> make_set(T, [H|Acc])
	end;
make_set([], Acc) ->
	lists:sort(Acc).

% Internal use for the tokenizer regex pattern only
escape("[") -> "\\[";
escape("]") -> "\\]";
escape("(") -> "\\(";
escape(")") -> "\\)";
escape(".") -> "\\.";
escape(C) -> C.


