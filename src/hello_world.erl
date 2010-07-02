-module(hello_world).
-export([initial/0, mutate/1, combine/2, fitness/1, to_phenotype/1]).
-define(LENGTH, 12).
-define(TARGET, "hello world!").
-define(CHARS, "abcdefghijklmnopqrstuvwxyz !").

initial() ->
    lists:map(
      fun(_) -> lists:nth(random:uniform(length(?CHARS)), ?CHARS) end,
      lists:seq(1, length(?TARGET))).

mutate(String) ->
    %% Choose a random character to mutate
    %% to a random value
    ReplacePosition = random:uniform(length(String)),
    mutate(String, 1, ReplacePosition, []).

mutate([_Hd| Rest], Pos, Pos, Acc) ->
    mutate(Rest, Pos + 1, Pos, [lists:nth(random:uniform(length(?CHARS)), ?CHARS) | Acc]);
mutate([Hd | Rest], Pos, ReplacePos, Acc) ->
    mutate(Rest, Pos + 1, ReplacePos, [Hd | Acc]);
mutate([], _, _, Acc) ->
    lists:reverse(Acc).


combine(Parent1, Parent2) ->
    SplicePosition = random:uniform(length(Parent1)),
    {Left, Right} = case random:uniform(2) of
			1 -> {Parent1, Parent2};
			2 -> {Parent2, Parent1}
		    end,
    lists:sublist(Left, 1, SplicePosition) ++ lists:sublist(Right, SplicePosition + 1, length(Parent2)).


fitness(String) ->
    %% The fitness will simply be the number of correct
    %% characters
    {_, Match} = lists:foldl(fun(Char, {Pos, Match}) ->
				     case lists:nth(Pos, ?TARGET) =:= Char  of
					 true ->
					     {Pos + 1, Match + 1};
					 false ->
					     {Pos + 1, Match}
				     end
			     end,
			     {1, 0},
			     String),
    Match / 12.
    %% %% This will be the distance ffrom 0 to N
    %% Distance = string_metrics:levenshtein(String, ?TARGET),
    %% %% We want the fitness the be between 0.0 and 1.0, and since 
    %% %% we know the max distance for this problem is 13
    %% 1 - Distance/13.

    
to_phenotype(Genotype) ->
    Genotype.
    

