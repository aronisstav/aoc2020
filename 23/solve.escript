#!/usr/bin/env escript
-mode(compile).

%% https://adventofcode.com/2020/day/23

main([A, N|Args]) ->
  Input = {[C - $0 || C <- A], list_to_integer(N)},
  Ans =
    case Args =:= ["2"] of
      false -> first(Input);
      true -> second(Input)
    end,
  io:format("~p~n", [Ans]).

%%--------------------------------------------------------------------
first({A, N}) ->
  F = crab_cups(A, N),
  [1|T] = rot(1, F),
  [C + $0 || C <- T].

crab_cups(A, 0) -> A;
crab_cups([F, A, B, C | R], N) ->
  T = target(n(F), [A, B, C]),
  [T|NR] = rot(T, R ++ [F]),
  [F|FR] = rot(F, [T, A, B, C | NR]),
  crab_cups(FR ++ [F], N - 1).

target(N, L) ->
  case lists:member(N, L) of
    false -> N;
    true -> target(n(N), L)
  end.

n(1) -> 9;
n(N) -> N - 1.      

rot(T, [T|_] = L) -> L;
rot(T, [N|R]) -> rot(T, R ++[N]).

%%--------------------------------------------------------------------
-define(MAX, 1_000_000).

second({Cs, N}) ->
  {C, Cups} = make_cups(Cs),
  {_, F} = fast_cups(C, Cups, N),
  #{1 := A} = F,
  #{A := B} = F,
  A * B.

make_cups([H|T]) ->
  Fold = fun(C, {P, Acc}) -> {C, Acc#{P => C}} end,
  {L, M} = lists:foldl(Fold, {H, #{}}, T),
  {F, X} = add_extras(L, M, 9, ?MAX),
  {H, X#{F => H}}.

add_extras(L, M, I, I) -> {L, M};
add_extras(L, M, I, J) -> add_extras(I + 1, M#{L => I + 1}, I + 1, J).

nh(1) -> ?MAX;
nh(N) -> N - 1.      

ftarget(N, N, B, C) -> ftarget(nh(N), N, B, C);
ftarget(N, A, N, C) -> ftarget(nh(N), A, N, C);
ftarget(N, A, B, N) -> ftarget(nh(N), A, B, N);
ftarget(N, _, _, _) -> N.

fast_cups(H, Cups, 0) -> {H, Cups};
fast_cups(H, Cups, I) ->
  #{H := A} = Cups,
  #{A := B} = Cups,
  #{B := C} = Cups,
  #{C := N} = Cups,
  T = ftarget(nh(H), A, B, C),
  #{T := X} = Cups,
  NCups =
    Cups
    #{ H => N
     , T => A
     , C => X
     },
  fast_cups(N, NCups, I - 1).
