#!/usr/bin/env escript

%% https://adventofcode.com/2020/day/4

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
  preprocess(Data, #{}, []).

preprocess([], Map, Acc) ->
  lists:reverse([Map|Acc]);
preprocess(["\n"|Rest], Map, Acc) ->
  preprocess(Rest, #{}, [Map|Acc]);
preprocess([Line|Rest], Map, Acc) ->
  NewMap = read_line(Line, Map),
  preprocess(Rest, NewMap, Acc).

read_line(Line, Map) ->
  case io_lib:fread(" ~c~c~c:~s", Line) of
    {ok, [A, B, C, Y], Rest} ->
      NewMap = Map#{list_to_atom(A ++ B ++ C) => Y},
      read_line(Rest, NewMap);
    _ -> Map
  end.

first(Input) ->
  length([P || P <- Input, is_valid(P)]).

is_valid(P) ->
  case P of
    #{ byr := _
     , iyr := _
     , eyr := _
     , hgt := _
     , hcl := _
     , ecl := _
     , pid := _
     } -> true;
    _ -> false
  end.

second(Input) ->
  length([P || P <- Input, is_valid_strict(P)]).

is_valid_strict(P) ->
  case P of
    #{ byr := BYR
     , iyr := IYR
     , eyr := EYR
     , hgt := HGT
     , hcl := HCL
     , ecl := ECL
     , pid := PID
     } ->
      try
        BYRN = list_to_integer(BYR),
        true = 1920 =< BYRN,
        true = BYRN =< 2002,
        IYRN = list_to_integer(IYR),
        true = 2010 =< IYRN,
        true = IYRN =< 2020,
        EYRN = list_to_integer(EYR),
        true = 2020 =< EYRN,
        true = EYRN =< 2030,
        true = is_valid_height(HGT),
        true = is_valid_hair(HCL),
        true = is_valid_eye(ECL),
        _ = list_to_integer(PID),
        9 = length(PID),
        true
      catch
        _:_ -> false
      end;
    _ -> false
  end.

is_valid_height(HGT) ->
  case io_lib:fread("~d~s", HGT) of
    {ok, [N, U], []} ->
      case U of
        "cm" -> 150 =< N andalso N =< 193;
        "in" -> 59 =< N andalso N =< 76;
        _ -> false
      end;
    _ -> false
  end.

is_valid_hair([$#|Hex]) ->
  case io_lib:fread("~#", "16#" ++ Hex) of
    {ok, [_], []} -> true;
    _ -> false
  end.

is_valid_eye(ECL) ->
  lists:member(ECL, ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]).
