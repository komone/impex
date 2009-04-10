%%
%% Tuple Importer/Exporter
%% Copyright (c) 2009 Steve Davis <steven.charles.davis@gmail.com>. All rights reserved.
%% LICENSE: Creative Commons Non-Commercial License V 3.0 
%% http://creativecommons.org/licenses/by-nc/3.0/us/
%%
-module(impex_parser, [Def, Tokenizer]).
-vsn("0.1").
-author('steve@simulacity.com').

-include("../include/impex.hrl").

-export([import/1, import_file/1, export/1, export_file/2]).

%%
import_file(File) ->
	{ok, Bin} = file:read_file(File),
	import(binary_to_list(Bin)).
%%	
import(Source) -> 
	{vsn, Version} = lists:keyfind(vsn, 1, ?MODULE:module_info(attributes)),
	impex:log(full, "IMPEX Parser Version ~p~n", [Version]),
	impex:log(full, "Using Tokenizer...~n~p~n", [Tokenizer]),
	impex:log(terse, "Tokenizing...~n", []),
	Tokens = tokenize(Source),
	impex:log(normal, "~p~n", [Tokens]),
	impex:log(terse, "~p tokens~n", [length(Tokens)]),
	impex:log(terse, "Parsing...~n", []),
	Result = 
		try
			impex:stats(clear),
			{ok, Value, []} = parse(Def#form.root, Tokens),
			{Def#form.type, Value}
		catch
			error:Reason -> 
				{error, invalid_format, Reason}
		end,
	{M, N} = impex:stats(),
	impex:log(terse, "Efficiency ~p%, (~p of ~p matches)~n", [trunc(M * 100 / (M + N)), M, M + N]),
	Result.

%%
export_file(Term, Filename) ->
	file:write_file(Filename, export(Term)).
	
% TODO!!!
export(_Term) ->
	{error, not_implemented}.

%%
%% Tokenizer
%%

%%
tokenize(Source) ->
	Tokens = re:split(Source, Tokenizer, [{return, list}]), 
	Tokens1 = clean(Tokens, []),
	apply_mods(Tokens1, Def#form.mods).

%clean([[]|Rest], Acc) ->
%	clean(Rest, Acc);
clean([Token|Rest], Acc) ->
	case trim(Token) of
	%re:run(Token, "^[\n\t\r ]*$") of
	[] -> clean(Rest, Acc);
%	"," -> clean(Rest, Acc); %% IMPL: STRIPS COMMAS - is this valid?
	CleanToken -> clean(Rest, [CleanToken|Acc])
	end;
clean([], Acc) ->
	lists:reverse(Acc).

%%
trim(String) -> 
	String1 = trim1(lists:reverse(String)),
	trim1(lists:reverse(String1)).
trim1([$\t|T]) -> trim1(T);
trim1([$\r|T]) -> trim1(T);
trim1([$\n|T]) -> trim1(T);
trim1([$ |T]) -> trim1(T);
trim1(T) -> T.	

%%
apply_mods(Tokens, [Rule|Rest]) ->
	NewTokens = 
		case Rule of
		{whitespace, strip} -> 
			[X || X <- Tokens, X =/= []];
		_ -> Tokens
		end,
	apply_mods(NewTokens, Rest);
apply_mods(Tokens, []) ->
	Tokens.

%%
%% Parser
%%

%%
parse(Type, Tokens) ->
	{Type, Patterns} = lists:keyfind(Type, 1, Def#form.defs),	
	impex:log(all, "[~p] parse ~p~n", [Type, lists:sublist(Tokens, 3) ++ ['...']]),
%	impex:log(full, "[~p] patterns ~p~n", [Type, Patterns]),
	case apply_patterns(Type, Patterns, Tokens) of
	{ok, Value, Rest} -> 
		Result = apply_transform(Type, Value),
		impex:log(full, "[~p] -> ~p ~p~n", [Type, Result, lists:sublist(Rest, 3) ++ ['...']]),
		{ok, Result, Rest};
	nomatch -> 
		impex:log(full, "[~p] nomatch ~p~n", [Type, Type]),
		nomatch
	end.

%% 
apply_patterns(Type, [H|T], Tokens) ->
	impex:log(full, "[~p] try ~p~n", [Type, H]),
	case apply_pattern(Type, H, Tokens, []) of
	{ok, Value, Rest} -> 
		impex:stats(match), 
		{ok, Value, Rest};
	nomatch -> 
		impex:log(normal, "[~p] nomatch ~p~n", [Type, H]),
		apply_patterns(Type, T, Tokens)
	end;
apply_patterns(_, [], _Tokens) ->
	nomatch.

%% First, check for terminals
apply_pattern(Type, [H|T], [Token|Rest], Acc) when H =:= Token ->
	impex:log(normal, "[~p] -> terminal ~p~n", [Type, Token]),
%	impex:stats(match), 
	apply_pattern(Type, T, Rest, Acc);
%% ...and then keywords
apply_pattern(Type, [{H}|T], [Token|Rest], Acc) when H =:= Token ->
	impex:log(normal, "[~p] -> keyword ~p~n", [Type, Token]),
	impex:stats(match), 
	apply_pattern(Type, T, Rest, [list_to_atom(Token)|Acc]); 
%% ...and then the regex function
apply_pattern(Type, [{regex, Regexp}|T], [Token|Rest], Acc) ->
	impex:log(full, "[~p] -> regex ~p~n", [Type, Token]),
	case is_match(Token, Regexp) of
	true -> 
		impex:stats(match), 
		Value = convert_to_type(Regexp, Token),
		apply_pattern(Type, T, Rest, [Value|Acc]);
	false -> 
		impex:stats(nomatch), 
		nomatch
	end;
% Then deal with list declarations
apply_pattern(Type, [{list, Separator, ListType}|T], Tokens, Acc) ->
	impex:log(full, "[~p] expect list of ~p~n", [Type, ListType]),
	case parse(ListType, Tokens) of
	{ok, Value, Rest} -> % the list should have at least one value
		impex:stats(match),
		{ok, Values, Rest1} = 
			repeat_pattern(Type, [Separator, ListType], Rest, [Value]),
		apply_pattern(Type, T, Rest1, [Values|Acc]);
	nomatch -> 
		impex:stats(nomatch), 
		nomatch
	end;
% Then non-recursive type declarations
apply_pattern(Type, [H|T], Tokens, Acc) when is_atom(H) ->
	impex:log(all, "[~p] expect ~p~n", [Type, H]),
	case parse(H, Tokens) of
	{ok, Value, Rest} -> 
		impex:stats(match), 
		impex:log(all, "[~p] resuming after ~p ~p ~p~n", [Type, H, T, Rest]),
		apply_pattern(Type, T, Rest, [Value|Acc]);
	nomatch -> 
		impex:stats(nomatch), 
		nomatch
	end;
%% Then, if the pattern is not consumed, the match failed
apply_pattern(_Type, [_|_], _Tokens, _Acc) ->
	impex:stats(nomatch), 
	nomatch;
%% Otherwise, the pattern was fully consumed and so the match succeeded
apply_pattern(_Type, [], Tokens, Acc) ->
	%% TODO: this avoids embedded lists in the output... 
	%% ...but is it really just a hack? YES -- but how to get rid of it?
	case Acc of
	[Value] -> {ok, Value, Tokens};
	_ -> {ok, lists:reverse(Acc), Tokens}
	end.

%%
repeat_pattern(Type, Pattern, Tokens, Acc) ->
	case apply_pattern(Type, Pattern, Tokens, []) of
	{ok, Value, Rest} -> 
		repeat_pattern(Type, Pattern, Rest, [Value|Acc]);
	nomatch ->
		impex:log(all, "End of ~p list ~p ~p~n", [Type, Acc, Tokens]),
		{ok, lists:reverse(Acc), Tokens}
	end.

%%
convert_to_type(number, Token) ->
	case is_match(Token, integer) of
	true -> list_to_integer(Token);
	false -> list_to_float(Token)
	end;
convert_to_type(string, Token) ->
	[[], Value, []] = re:split(Token, "\"", [{return, list}]),
	Value;
convert_to_type(_, Token) -> Token.

%% 
apply_transform(Type, Value) ->
	case lists:keyfind(Type, 1, Def#form.mods) of 
	{Type, Function} when is_function(Function, 1) ->
		impex:log(full, "[~p] transform ~p ->", [Type, Value]),
		Result = 
			try 
				Function(Value)
			catch
				error:Reason -> exit({invalid_fun, Reason, Type, Value})
			end,
		impex:log(full, " ~p~n", [Result]),
		Result;
	{Type, atom, _} when is_list(Value) -> list_to_atom(Value);
	{Type, tuple, Size} when is_list(Value), length(Value) =:= Size -> list_to_tuple(Value);
	{Type, list, Size} when is_tuple(Value), size(Value) =:= Size -> tuple_to_list(Value);	
	{Type, list, _} when is_atom(Value) -> atom_to_list(Value);	
	_ -> Value
	end.

%%
%% Regular Expressions
%%
is_match(Token, Regex) ->
	Expr = lists:append(["^", impex:regex(Regex), "$"]),
	case re:run(Token, Expr) of
	match -> true;
	{match, _} -> true;
	nomatch -> false
	end.
