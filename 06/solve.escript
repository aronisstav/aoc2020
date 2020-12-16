#!/usr/bin/env escript

%% https://adventofcode.com/2020/day/6

-mode(compile).

main(Args) ->
  Input = read_list(),
  Ans =
    case Args of
      ["2"] -> second(Input);
      _ -> first(Input)
    end,
  io:format("~w~n", [Ans]).

read_list() ->
  Data = read_list([]),
  preprocess(Data).

read_list(Acc) ->
  case io:get_line("") of
    eof ->
      Fold = fun(L, R) -> [L|R] end,
      lists:foldl(Fold, [], Acc);
    Res -> read_list([Res|Acc])
  end.

preprocess(Data) ->
  preprocess(Data, new_sets(), []).

preprocess([], Map, Acc) ->
  lists:reverse([Map|Acc]);
preprocess(["\n"|Rest], Map, Acc) ->
  preprocess(Rest, new_sets(), [Map|Acc]);
preprocess([Line|Rest], Map, Acc) ->
  NewMap = read_line(Line, Map),
  preprocess(Rest, NewMap, Acc).

new_sets() ->
  U = sets:new(),
  I = sets:from_list(lists:seq($a, $z)),
  {U, I}.

read_line(Line, {U, I}) ->
  case io_lib:fread(" ~s", Line) of
    {ok, [S], Rest} ->
      Set = sets:from_list(S),
      NU = sets:union(Set, U),
      NI = sets:intersection(Set, I),
      read_line(Rest, {NU, NI});
    _ -> {U, I}
  end.

first(Input) ->
  Fold = fun({U, _I}, Acc) -> Acc + sets:size(U) end,
  lists:foldl(Fold, 0, Input).

second(Input) ->
  Fold = fun({_U, I}, Acc) -> Acc + sets:size(I) end,
  lists:foldl(Fold, 0, Input).
