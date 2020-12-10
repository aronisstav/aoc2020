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
  %% correctly can be made when starting with a given adapter.
  [Last|RevSort] = lists:reverse([0|lists:sort(Input)]),
  Fold =
    fun(H, M) ->
        %% Starting with H, any sequence whose first adapter is either
        %% 1, 2, or 3 steps away can be used to finish correctly.
        M#{H => lists:sum([maps:get(H + D, M, 0) || D <- [1, 2, 3]])}
    end,
  %% Only one sequence from the last adapter exists.  How many
  %% sequences can be made starting with 0?
  maps:get(0 ,lists:foldl(Fold, #{Last => 1}, RevSort)).
