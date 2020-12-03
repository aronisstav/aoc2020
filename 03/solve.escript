#!/usr/bin/env escript

%% https://adventofcode.com/2020/day/3

-mode(compile).

main(Args) ->
  Input = read_list("~s"),
  Ans =
    case Args of
      ["2"] -> second(Input);
      _ -> first(Input)
    end,
  io:format("~w~n", [Ans]).

read_list(Pat) -> read_list(Pat, []).

read_list(Pat, Acc) ->
  case io:fread("", Pat) of
    {ok, Res} -> read_list(Pat, [Res|Acc]);
    eof ->
      Fold = fun([L], R) -> [L|R] end,
      lists:foldl(Fold, [], Acc)
  end.

first(Input) ->
  count(Input, 3, 1).

second(Input) ->
  Fold = fun(N, A) -> N * A end,
  Slopes = [{1,1}, {3,1}, {5,1}, {7,1}, {1,2}],
  lists:foldl(Fold, 1, [count(Input, R, D) || {R, D} <- Slopes]).

count([L|_] = Lines, H, V) ->
  first(Lines, H, V, 1, length(L), 0).

first(   [], _, _, _, _, N) -> N;
first([C|R], H, V, I, L, N) ->
  Inc =
    case lists:nth(I, C) of
      $# -> 1;
      $. -> 0
    end,
  II = ((I + H - 1) rem L) + 1,
  NN = N + Inc,
  RR =
    case V of
      1 -> R;
      2 -> maybe_tl(R)
    end,
  first(RR, H, V, II, L, NN).

maybe_tl([]) -> [];
maybe_tl([_|R]) -> R.
