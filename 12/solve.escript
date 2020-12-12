#!/usr/bin/env escript
-mode(compile).

%% https://adventofcode.com/2020/day/12

main(Args) ->
  Input = read_list("~c~d"),
  Ans =
    case Args =:= ["2"] of
      false -> first(Input);
      true -> second(Input)
    end,
  io:format("~w~n", [Ans]).

%%--------------------------------------------------------------------
read_list(Pat) -> read_list(Pat, []).

read_list(Pat, Acc) ->
  case io:fread("", Pat) of
    {ok, Res} -> read_list(Pat, [Res|Acc]);
    eof ->
      Fold = fun([[C],D], R) -> [{C, D}|R] end,
      lists:foldl(Fold, [], Acc)
  end.

%%--------------------------------------------------------------------
first(Input) ->
  Fold = fun(Op, P) -> move(Op, P) end,
  {E, N, _} = lists:foldl(Fold, {0, 0, $E}, Input),
  abs(E) + abs(N).

move({$F, X}, {_, _, D} = P) -> move({D, X}, P);
move({$N, X}, {E, N, D}) -> {E, N + X, D};
move({$S, X}, {E, N, D}) -> {E, N - X, D};
move({$E, X}, {E, N, D}) -> {E + X, N, D};
move({$W, X}, {E, N, D}) -> {E - X, N, D};
move(Op, {E, N, D}) -> {E, N, turn(Op, D)}.

turn({_, 0}, D) -> D;
turn({$R, 90}, $N) -> $E;
turn({$R, 90}, $E) -> $S;
turn({$R, 90}, $S) -> $W;
turn({$R, 90}, $W) -> $N;
turn({$R, X}, D) -> turn({$R, X - 90}, turn({$R, 90}, D));
turn({$L, X}, D) -> turn({$R, 360 - X}, D).

%%--------------------------------------------------------------------
second(Input) ->
  Fold = fun(Op, P) -> wp_move(Op, P) end,
  {{E, N}, _} = lists:foldl(Fold, {{0, 0}, {10, 1}}, Input),
  abs(E) + abs(N).

wp_move({$F, X}, {{E, N}, {WE, WN} = WP}) -> {{E + X * WE, N + X * WN}, WP};
wp_move({$N, X}, {P, {WE, WN}}) -> {P, {WE, WN + X}};
wp_move({$S, X}, {P, {WE, WN}}) -> {P, {WE, WN - X}};
wp_move({$E, X}, {P, {WE, WN}}) -> {P, {WE + X, WN}};
wp_move({$W, X}, {P, {WE, WN}}) -> {P, {WE - X, WN}};
wp_move(Op, {P, WP}) -> {P, wp_turn(Op, WP)}.

wp_turn({_, 0}, WP) -> WP;
wp_turn({$R, 90}, {WE, WN}) -> {WN, -WE};
wp_turn({$R, X}, WP) -> wp_turn({$R, X - 90}, wp_turn({$R, 90}, WP));
wp_turn({$L, X}, WP) -> wp_turn({$R, 360 - X}, WP).
