#!/usr/bin/env escript
-mode(compile).

%% https://adventofcode.com/2020/day/14

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
  read_prog([]).

read_prog(Acc) ->
  case io:fread("", "m~c") of
    eof -> lists:reverse(Acc);
    {ok, [C]} ->
      case C of
        "a" ->
          {ok, [MaskStr]} = io:fread("", "sk = ~s"),
          read_prog([{mask, MaskStr}|Acc]);
        "e" ->
          {ok, [I, V]} = io:fread("", "m[~d] = ~d"),
          read_prog([{upd, {I, V}}|Acc])
      end
  end.

%%--------------------------------------------------------------------
first(Prog) ->
  Mem = init(Prog),
  Fold = fun(_, V, A) -> V + A end,
  maps:fold(Fold, 0, Mem).

init(Prog) ->
  Fold =
    fun({upd, {I, V}}, {And, Or, Acc}) ->
        FV = (V bor Or) band And,
        {And, Or, Acc#{I => FV}};
       ({mask, MaskStr}, {_, _, Acc}) ->
        {And, Or} = to_masks(MaskStr),
        {And, Or, Acc}
    end,
  {_, _, Mem} = lists:foldl(Fold, {1, 0, #{}}, Prog),
  Mem.

to_masks(MaskStr) ->
  to_masks(MaskStr, 1, 0, 0).

to_masks([], _, And, Or) ->
  {And, Or};
to_masks([H|T], N, And, Or) ->
  {NAnd, NOr} =
    case H of
      $X -> {2 * And + 1, 2 * Or};
      $1 -> {2 * And + 1, 2 * Or + 1};
      $0 -> {2 * And, 2 * Or}
    end,
  to_masks(T, 2*N, NAnd, NOr).

%%--------------------------------------------------------------------
second(Prog) ->
  Mem = init2(Prog),
  Fold = fun(_, V, A) -> V + A end,
  maps:fold(Fold, 0, Mem).

init2(Prog) ->
  Fold =
    fun({upd, {I, V}}, {MaskStr, Acc}) ->
        FoldI = fun(A, IAcc) -> IAcc#{A=>V} end,
        {MaskStr, lists:foldl(FoldI, Acc, addresses(MaskStr, I))};
       ({mask, MaskStr}, {_, Acc}) ->
        {MaskStr, Acc}
    end,
  {_, Mem} = lists:foldl(Fold, {"", #{}}, Prog),
  Mem.

addresses(MaskStr, Addr) ->
  Fold =
    fun(M, {I, N, Addrs}) ->
        NI = I div 2,
        ID = I rem 2,
        NN = N * 2,
        NAddr =
          case M of
            $0 -> [A + ID * N || A <- Addrs];
            $1 -> [A + 1 * N  || A <- Addrs];
            $X -> [A + X * N  || A <- Addrs, X <- [0, 1]]
          end,
        {NI, NN, NAddr}
    end,
  {_, _, Addrs} = lists:foldl(Fold, {Addr, 1, [0]}, lists:reverse(MaskStr)),
  Addrs.
