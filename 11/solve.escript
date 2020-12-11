#!/usr/bin/env escript
-mode(compile).

%% https://adventofcode.com/2020/day/11

main(Args) ->
  Input = read_list("~s"),
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
      Fold = fun([L], {R, _, G}) -> add_line(L, R + 1, 1, G) end,
      lists:foldl(Fold, {0, 1, #{}}, lists:reverse(Acc))
  end.

add_line([], R, C, G) -> {R, C - 1, G};
add_line([H|T], R, C, G) ->
  add_line(T, R, C + 1, G#{{R,C} => H}).

maybe_print({RL, CL, G}) ->
  case os:getenv("PRINT") =:= false of
    true -> ok;
    false ->
      ForeachL =
        fun(R) ->
            ForeachC = fun(C) -> io:format("~c", [maps:get({R, C}, G)]) end,
            lists:foreach(ForeachC, lists:seq(1, CL)),
            io:format("~n")
        end,
      lists:foreach(ForeachL, lists:seq(1, RL)),
      io:format("~n")
  end.

%%--------------------------------------------------------------------
first(Input) ->
  Stable = evolve_fix(Input, first),
  count($#, Stable).

evolve_fix(Grid, Mode) ->
  maybe_print(Grid),
  {Evolved, NewGrid} = evolve(Grid, Mode),
  case Evolved of
    false -> Grid;
    true -> evolve_fix(NewGrid, Mode)
  end.

evolve({R, C, Grid}, Mode) ->
  Fold =
    fun(K, V, {E, NG}) ->
        N = count_neighs(K, Grid, Mode),
        NV =
          case {V, N, Mode} of
            {$L, 0,      _} -> $#;
            {$#, X,  first} when X > 3 -> $L;
            {$#, X, second} when X > 4 -> $L;
            _ -> V
          end,
        {E orelse V =/= NV, NG#{K => NV}}
    end,
  {Evolved, NG} = maps:fold(Fold, {false, #{}}, Grid),
  {Evolved, {R, C, NG}}.

count_neighs(P, Grid, Mode) ->
  Dirs =
    [{RO, CO} ||
      RO <- [-1, 0, 1],
      CO <- [-1, 0, 1],
      RO =/= 0 orelse CO =/= 0],
  lists:sum([occupied(P, D, Grid, Mode) || D <- Dirs]).

occupied({R, C}, {RO, CO} = D, Grid, Mode) ->
  NP = {R + RO, C + CO},
  case {maps:get(NP, Grid, $X), Mode} of
    {$#,     _} -> 1;
    {$., first} -> 0;
    {$.,     _} -> occupied(NP, D, Grid, Mode);
    _ -> 0
  end.

count(X, {_, _, G}) ->
  Fold =
    fun(_, V, C) when V =:= X -> C + 1;
       (_, _, C) -> C
    end,
  maps:fold(Fold, 0, G).

%%--------------------------------------------------------------------
second(Input) ->
  Stable = evolve_fix(Input, second),
  count($#, Stable).
