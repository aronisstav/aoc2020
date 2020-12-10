#!/usr/bin/env escript
-mode(compile).

%% https://adventofcode.com/2020/day/10

main(Args) ->
  Input = read_list("~d"),
  Ans =
    case Args =:= ["2"] of
      false -> first(Input);
      true -> second(Input)
    end,
  io:format("~w~n", [Ans]).

%%--------------------------------------------------------------------
read_list(Pat) -> read_list(Pat, []).

read_list(Pat, Acc) ->
  case io:fread("", Pat) of
    {ok, Res} -> read_list(Pat, [Res|Acc]);
    eof ->
      Fold = fun(L, R) -> lists:append(L, R) end,
      lists:foldl(Fold, [], Acc)
  end.

%%--------------------------------------------------------------------
first(Input) ->
  Sorted = lists:sort(Input),
  jumps(Sorted, 0, 0, 0).

jumps([], _V, One, Three) -> One * (Three + 1);
jumps([H|R], V, One, Three) ->
  {NOne, NThree} = 
    case H - V of
      1 -> {One + 1, Three};
      3 -> {One, Three + 1};
      _ -> {One, Three}
    end,
  jumps(R, H, NOne, NThree).

%%--------------------------------------------------------------------
second(Input) ->
  %% Solve this backwards, tracking how many sequences that finish
  %% correctly we can make starting with a given adapter.
  [H|RevSort] = lists:reverse([0|lists:sort(Input)]),
  %% There is only one sequence from the last one (i.e. H -> H + 3).
  M = #{H => 1},
  combi(RevSort, M).

%% "How many sequences can we make starting with 0?"
combi([], #{0 := V}) -> V;
combi([H|T], M) ->
  %% Starting with H, we can make all sequences whose next adapter is
  %% either 1, 2, or 3 steps away.  The nice property here is that if
  %% we don't use an adapter, we continue with a higher one and we can
  %% therefore consider each choice only once!
  V = lists:sum([maps:get(H + D, M, 0) || D <- [1, 2, 3]]),
  combi(T, M#{H => V}).
