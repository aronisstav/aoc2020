#!/usr/bin/env escript
-mode(compile).

%% https://adventofcode.com/2020/day/21

main(Args) ->
  Input = read_input(),
  Ans =
    case Args =:= ["2"] of
      false -> first(Input);
      true -> second(Input)
    end,
  io:format("~p~n", [Ans]).

%%--------------------------------------------------------------------
read_input() ->
  Lines = read_lines([]),
  parse(Lines).

read_lines(Acc) ->
  case io:get_line("") of
    eof ->
      Fold = fun(S, R) -> [S|R] end,
      lists:foldl(Fold, [], Acc);
    Res -> read_lines([Res|Acc])
  end.

parse(Lines) ->
  Fold =
    fun(L, Acc) ->
        [IngrStr, AllergenStrNL] = string:split(L, " (contains "),
        [AllergenStr, _] = string:split(AllergenStrNL, ")\n"),
        Ingrs = string:split(IngrStr, " ", all),
        Allergens = string:split(AllergenStr, ", ", all),
        [{Ingrs, Allergens}|Acc]
    end,
  lists:foldr(Fold, [], Lines).          

%%--------------------------------------------------------------------
first(Input) ->
  FromFood = from_food(Input, #{}),
  Solved = solve_food(FromFood).

from_food(Food, Map) ->
  FoldF =
    fun({Ingr, Allergens}, FAcc) ->
        IngrSet = ordsets:from_list(Ingr),
        Update = fun(V) -> ordsets:intersection(V, IngrSet) end,
        FoldA = fun(A, Acc) -> maps:update_with(A, Update, IngrSet, Acc) end,
        lists:foldl(FoldA, FAcc, Allergens)
    end,
  lists:foldl(FoldF, Map, Food).

solve_food(Map) ->
  solve(#{a => Map, k => #{}}).

solve(Maps) ->
  NewMaps = solve_once(Maps),
  case NewMaps =:= Maps of
    true -> NewMaps;
    false -> solve(NewMaps)
  end.

solve_once(#{a := All, k := Known}) ->
  Fold =
    fun(A, Is, {AAcc, KAcc}) ->
        {NV, NKAcc} =
          case Is of
            [I] -> {I, KAcc#{I => A}};
            _ -> {rem_known(Is, KAcc), KAcc}
          end,
        {AAcc#{A => NV}, NKAcc}
    end,
  {NAll, NKnown} = maps:fold(Fold, {#{}, Known}, All),
  #{a => NAll, k => NKnown}.

rem_known(Is, Known) ->
  Vs = maps:keys(Known),
  Is -- Vs.

%%--------------------------------------------------------------------
second(Input) ->
  Input.
