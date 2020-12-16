#!/usr/bin/env escript

%% https://adventofcode.com/2020/day/1

-mode(compile).

main(Args) ->
  Nums = lists:sort(read_list("~d")),
  {ok, Ans} =
    case Args of
      ["2"] -> solve3(Nums);
      _ -> findsum(2021, 1, Nums)
    end,
  io:format("~w~n", [Ans]).

read_list(Pat) -> read_list(Pat, []).

read_list(Pat, Acc) ->
  case io:fread("", Pat) of
    {ok, Res} -> read_list(Pat, [Res|Acc]);
    eof -> lists:append(lists:reverse(Acc))
  end.

%% This is cubic, but quadratic complexity can be achieved by
%% inserting the numbers in a map and then for every pair checking
%% whether the map contains GOAL - S[i] - S[j]!
solve3([_, _]) -> -1;
solve3([H|T]) -> 
  case solve2(H, T) of
    {ok, V} -> {ok, V};
    none -> solve3(T)
  end.

solve2(_, [_]) -> none;
solve2(A, [H|T]) -> 
  case findsum(2020 - A, H, T) of
    {ok, V} -> {ok, A * H * V};
    none -> solve2(A, T)
  end.

findsum(_, _, []) -> none;
findsum(Sum, A, [H|T]) ->
  if A + H  > Sum  -> findsum(Sum, A, T);
     A + H =:= Sum -> {ok, H};
     A + H  < Sum  -> none
  end.
