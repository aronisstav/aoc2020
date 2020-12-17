#!/usr/bin/env escript
-mode(compile).

%% https://adventofcode.com/2020/day/17

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
      Fold = fun([L], {R, _, G}) -> add_line(L, R + 1, 0, G) end,
      {R, C, M} = lists:foldl(Fold, {-1, 0, #{}}, lists:reverse(Acc)),
      {{0, R}, {0, C}, {0, 0}, {0, 0}, M}
  end.

add_line([], R, C, G) -> {R, C - 1, G};
add_line([H|T], R, C, G) ->
  add_line(T, R, C + 1, G#{{R, C, 0, 0} => H}).

maybe_print({{YL, YH}, {XL, XH}, {ZL, ZH}, {WL, WH}, G}) ->
  case os:getenv("PRINT") =:= false of
    true -> ok;
    false ->
      ForeachW =
        fun(W) ->
            io:format("w = ~p~n",[W]),            
            ForeachZ =
              fun(Z) ->
                  io:format("z = ~p~n",[Z]),
                  ForeachL =
                    fun(R) ->
                        ForeachC =
                          fun(C) ->
                              A = maps:get({R, C, Z, W}, G),
                              io:format("~c", [A])
                          end,
                        lists:foreach(ForeachC, lists:seq(XL, XH)),
                        io:format("~n")
                    end,
                  lists:foreach(ForeachL, lists:seq(YL, YH)),
                  io:format("~n")
              end,
            lists:foreach(ForeachZ, lists:seq(ZL, ZH)),
            io:format("~n")
        end,
      lists:foreach(ForeachW, lists:seq(WL, WH)),
      io:format("~n")
  end.

%%--------------------------------------------------------------------
first(Input) ->
  maybe_print(Input),
  Final = evolve(6, Input, first),
  count(Final, $#).

evolve(0, Grid, _) ->
  Grid;
evolve(N, Grid, Mode) ->
  NewGrid = evolve(Grid, Mode),
  maybe_print(NewGrid),
  evolve(N - 1, NewGrid, Mode).

evolve({{YL, YH}, {XL, XH}, {ZL, ZH}, {WL, WH}, M}, Mode) ->
  {WLL, WHH} = NW =
    case Mode of
      first -> {WL, WH};
      second -> {WL - 1, WH + 1}
    end,
  FoldW =
    fun(W, AW) ->
        FoldZ =
          fun(Z, AZ) ->
              FoldY =
                fun(Y, AY) ->
                    FoldX =
                      fun(X, AX) ->
                          Neighs = neighs(Y, X, Z, W, M),
                          NV =
                            case maps:get({Y, X, Z, W}, M, $.) of
                              $# when Neighs =:= 2; Neighs =:= 3 -> $#;
                              $# -> $.;
                              $. when Neighs =:= 3 -> $#;
                              $. -> $.
                            end,
                          AX#{{Y,X,Z,W} => NV}
                      end,
                    lists:foldl(FoldX, AY, lists:seq(XL - 1, XH + 1))
                end,
              lists:foldl(FoldY, AZ, lists:seq(YL - 1, YH + 1))
          end,
        lists:foldl(FoldZ, AW, lists:seq(ZL - 1, ZH + 1))
    end,
  NM = lists:foldl(FoldW, #{}, lists:seq(WLL, WHH)),
  NY = {YL - 1, YH + 1},
  NX = {XL - 1, XH + 1},
  NZ = {ZL - 1, ZH + 1},
  {NY, NX, NZ, NW, NM}.

neighs(Y, X, Z, W, M) ->
  Cs =
    [{Y + YO, X + XO, Z + ZO, W + WO} ||
      YO <- [-1, 0, 1],
      XO <- [-1, 0, 1],
      ZO <- [-1, 0, 1],
      WO <- [-1, 0, 1],
      YO =/= 0 orelse XO =/= 0 orelse ZO =/= 0 orelse WO =/= 0
    ],
  Fold =
    fun(C, A) ->
        case maps:get(C, M, $.) of
          $# -> A + 1;
          $. -> A
        end
    end,
  lists:foldl(Fold, 0, Cs).

count({_, _, _, _, M}, S) ->
  Fold =
    fun(_, I, Acc) ->
        case S =:= I of
          true -> Acc + 1;
          false -> Acc
        end
    end,
  maps:fold(Fold, 0, M).        

%%--------------------------------------------------------------------
second(Input) ->
  maybe_print(Input),
  Final = evolve(6, Input, second),
  count(Final, $#).
