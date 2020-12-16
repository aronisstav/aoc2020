#!/usr/bin/env escript

%% https://adventofcode.com/2020/day/5

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
  higest(Input).

second(Input) ->
  missing(maps:from_list([{get_id(I), ok} || I <- Input])).

higest(Input) ->
  lists:max([get_id(I) || I <- Input]).

get_id(BP) ->
  Id = get_id(BP, {0, 127, 0, 7}),
  %% io:format("~n~p~n~n", [Id]),
  Id.

get_id([], {VL, VL, HL, HL}) ->
  VL * 8 + HL;
get_id([C|R], {VL, VH, HL, HH}) ->
  VP = (VH - VL) div 2,
  NVL = VL + VP + 1,
  NVH = VL + VP,
  HP = (HH - HL) div 2,
  NHL = HL + HP + 1,
  NHH = HL + HP,
  NL =
    case C of
      $F -> { VL, NVH,  HL,  HH};
      $B -> {NVL,  VH,  HL,  HH};
      $L -> { VL,  VH,  HL, NHH};
      $R -> { VL,  VH, NHL,  HH}
    end,
  %% io:format("~p~n", [NL]),
  get_id(R, NL).

missing(Map) ->
  missing(1, Map).

missing(N, Map) ->
  NL = N - 1,
  NH = N + 1,
  Hit =
    case Map of
      #{N := ok} -> false;
      #{ NL := ok
       , NH := ok
       } -> true;
      _ -> false
    end,
  case Hit of
    true -> N;
    false -> missing(N + 1, Map)
  end.
