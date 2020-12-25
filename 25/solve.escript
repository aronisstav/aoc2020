#!/usr/bin/env escript
-mode(compile).

%% https://adventofcode.com/2020/day/25

-define(m, 20201227).
-define(sn, 7).

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
  {ok, [D, K]} = io:fread("", "~d ~d"),
  {D, K}.

%%--------------------------------------------------------------------
first({D, K}) ->
  L = break(D),
  iter(L, K, 1).

break(T) ->
  break(1, T, 0).

break(T, T, L) -> L;
break(I, T, L) -> break(I * ?sn rem ?m, T, L + 1).

iter(0, _S, N) -> N;
iter(I, S, N) -> iter(I - 1, S, N * S rem ?m).

%%--------------------------------------------------------------------
second(I) ->
  I.
