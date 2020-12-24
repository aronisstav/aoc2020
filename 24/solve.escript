#!/usr/bin/env escript
-mode(compile).

%% https://adventofcode.com/2020/day/24

main(Args) ->
  Input = read_input(),
  Ans =
    case Args =:= ["2"] of
      false -> first(Input);
      true -> second(Input)
    end,
  io:format("~p~n", [Ans]).

%%--------------------------------------------------------------------
read_input() -> read_lines([]).

read_lines(Acc) ->
  case io:get_line("") of
    eof -> lists:reverse(Acc);
    Line ->
      L = read_line(Line, []),
      read_lines([L|Acc])
  end.

read_line("\n", Acc) -> lists:reverse(Acc);
read_line("e" ++ R, Acc) -> read_line(R, [e|Acc]);
read_line("w" ++ R, Acc) -> read_line(R, [w|Acc]);
read_line("ne" ++ R, Acc) -> read_line(R, [ne|Acc]);
read_line("nw" ++ R, Acc) -> read_line(R, [nw|Acc]);
read_line("se" ++ R, Acc) -> read_line(R, [se|Acc]);
read_line("sw" ++ R, Acc) -> read_line(R, [sw|Acc]).

%%--------------------------------------------------------------------
first(Input) ->
  life(Input, 0).

life(Input, N) ->
  Fold = fun(I, Acc) -> flip(I, Acc) end,
  M = lists:foldl(Fold, #{}, Input),
  FM = play(M, N),
  length([black || black <- maps:values(FM)]).

flip(Line, Map) ->
  K = get_c(Line),
  case maps:get(K, Map, white) of
    white -> Map#{K => black};
    black -> maps:remove(K, Map)
  end.

get_c(Line) ->
  Fold = fun move/2,
  lists:foldl(Fold, {0, 0, 0}, Line).

move(e, {X, Y, Z}) -> {X + 1, Y - 1, Z};
move(w, {X, Y, Z}) -> {X - 1, Y + 1, Z};
move(ne, {X, Y, Z}) -> {X + 1, Y, Z - 1};
move(sw, {X, Y, Z}) -> {X - 1, Y, Z + 1};
move(nw, {X, Y, Z}) -> {X, Y + 1, Z - 1};
move(se, {X, Y, Z}) -> {X, Y - 1, Z + 1}.

%%--------------------------------------------------------------------
second(Input) ->
  life(Input, 100).

play(M, 0) -> M;
play(M, N) ->
  L = limit(M) + 1,
  R = lists:seq(-L, L),
  Cs = [{X, Y, Z} || X <- R, Y <- R, Z <- R, X + Y + Z =:= 0],
  play(steps(Cs, M), N - 1).

limit(M) ->
  Fold = fun({X, Y, Z}, Max) -> lists:max([Max|[abs(I) || I <- [X, Y, Z]]]) end,
  lists:foldl(Fold, 0, maps:keys(M)).

steps(Cs, M) ->
  Fold =
    fun(C, Acc) ->
        case step(C, M) of
          black -> Acc#{C => black};
          white -> Acc
        end
    end,
  lists:foldl(Fold, #{}, Cs).

step(C, M) ->
  case {maps:get(C, M, white), neighs(C, M)} of
    {black, 0} -> white;
    {black, N} when N > 2 -> white;
    {white, 2} -> black;
    {Color, _} -> Color
  end.

neighs(C, N) ->
  Cs = [maps:get(move(D, C), N, white) || D <- [e, w, ne, sw, nw, se]],
  length([black || black <- Cs]).
