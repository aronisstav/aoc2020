#!/usr/bin/env escript
%%! -pa ../vm
-mode(compile).

%% https://adventofcode.com/2020/day/8

main([File|Args]) ->
  vm:remake(),
  Input = vm:load(File),
  Ans =
    case Args of
      ["2"] -> second(Input);
      _ -> first(Input)
    end,
  io:format("~w~n", [Ans]).

first(Input) ->
  #{acc := Acc} = vm:run(Input, #{run_once => true}),
  Acc.

second(Input) ->
  try_corrupt(Input, 0).

try_corrupt(Code, N) ->
  case corrupt(Code, N) of
    {ok, NewCode} ->
      case vm:run(NewCode, #{run_once => true}) of
        #{acc := Acc, cp := CP, size := S} when CP =:= S ->
          Acc;
        _ -> try_corrupt(Code, N + 1)
      end;
    _ -> try_corrupt(Code, N + 1)
  end.

corrupt(Code, N) ->
  case Code of
    #{N := {nop, X}} -> {ok, Code#{N => {jmp, X}}};
    #{N := {jmp, X}} -> {ok, Code#{N => {nop, X}}};
    _ -> false
  end.
