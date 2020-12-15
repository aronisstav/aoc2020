#!/usr/bin/env escript
-mode(compile).

%% https://adventofcode.com/2020/day/15

main(Args) ->
  Input = read_input(),
  Ans =
    case Args =:= ["2"] of
      false -> first(Input);
      true -> second(Input)
    end,
  io:format("~w~n", [Ans]).

%%--------------------------------------------------------------------
read_input() ->
  {ok, [S]} = io:fread("", "~s"),
  [list_to_integer(N) || N <- string:split(S, ",", all)].

%%--------------------------------------------------------------------
first(Input) ->
  common(Input, 2020).

common(Input, Goal) ->
  Fold = fun(X, {N, A}) -> {N + 1, [{X, N}|A]} end,
  {N, [{Last, _}|Ns]} = lists:foldl(Fold, {1, []}, Input),
  speak(N, Goal, Last, maps:from_list(Ns)).

speak(I, N, Last, _Map) when I > N -> Last;
speak(I, N, Last, Map) ->
  NLast =
    case maps:get(Last, Map, -1) of
      -1 -> 0;
      X -> I - X - 1
    end,
  speak(I + 1, N, NLast, Map#{Last => I - 1}).

%%--------------------------------------------------------------------
second(Input) ->
  common(Input, 30000000).
