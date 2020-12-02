#!/usr/bin/env escript

%% https://adventofcode.com/2020/day/2

main(Args) ->
  Input = lists:sort(read_list("~d-~d ~c: ~s")),
  Ans =
    case Args of
      ["2"] -> check(fun(X) -> isOkP2(X) end, Input);
      _ -> check(fun(X) -> isOkP1(X) end, Input)
    end,
  io:format("~w~n", [Ans]).

read_list(Pat) -> read_list(Pat, []).

read_list(Pat, Acc) ->
  case io:fread("", Pat) of
    {ok, Res} -> read_list(Pat, [Res|Acc]);
    eof ->
      Fold = fun(L, R) -> [list_to_tuple(L)|R] end,
      lists:foldl(Fold, [], Acc)
  end.

check(Fun, Pass) ->
  check(Fun, Pass, 0).

check(  _,       [], N) -> N;
check(Fun, [Pass|R], N) ->
  case Fun(Pass) of
    true -> check(Fun, R, N + 1);
    false -> check(Fun, R, N)
  end.

isOkP1({Min, Max, [C], Str}) ->
  N = length([L || L <- Str, L =:= C]),
  Min =< N andalso N =< Max.

isOkP2({P1, P2, [C], Str}) ->
  C1 = lists:nth(P1, Str),
  C2 = lists:nth(P2, Str),
  (C1 =:= C) xor (C2 =:= C).
