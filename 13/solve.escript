#!/usr/bin/env escript
-mode(compile).

%% https://adventofcode.com/2020/day/13

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
  {ok, [Est]} = io:fread("", "~d"),
  [S] = read_list("~s", fun([S], Acc) -> [S|Acc] end),
  Bus = [convert(B) || B <- string:split(S, ",", all)],
  {Est, Bus}.

read_list(Pat, Fold) -> read_list(Pat, Fold, []).

read_list(Pat, Fold, Acc) ->
  case io:fread("", Pat) of
    {ok, Res} -> read_list(Pat, Fold, [Res|Acc]);
    eof -> lists:foldl(Fold, [], Acc)
  end.

convert("x") -> x;
convert(Num) -> list_to_integer(Num).

%%--------------------------------------------------------------------
first({Est, Bus}) ->
  Nums = [X || X <- Bus, X =/= x],
  {B, Earliest} = earliest(Nums, Est, {-1, Est * Est}),
  B * Earliest.

earliest([], _, Min) -> Min;
earliest([H|T], Est, {_, M} = Min) ->
  Bus = ceil(Est / H) * H - Est,
  NewMin =
    case Bus < M of
      true -> {H, Bus};
      false -> Min
    end,
  earliest(T, Est, NewMin).

%%--------------------------------------------------------------------
second({_, Bus}) ->
  is_rel_prime([X || X <- Bus, X =/= x]),
  L = lists:reverse(lists:sort(convert_to_rem(Bus))),
  crt(L).

is_rel_prime([]) -> ok;
is_rel_prime([A|T]) ->
  Foreach = 
    fun(B) ->
        case gcd(A, B) of
          1 -> ok;
          GCD -> error({not_relatively_prime, #{a => A, b => B, gcd => GCD}})
        end
    end,
  lists:foreach(Foreach, T),
  is_rel_prime(T).

gcd(A, 0) -> A;
gcd(A, B) -> gcd(B, A rem B).

convert_to_rem(B) ->
  Fold =
    fun(x, {N, Acc}) -> {N + 1, Acc};
       (X, {N, Acc}) -> {N + 1, [{X, (X - (N rem X)) rem X}|Acc]}
    end,
  {_, L} = lists:foldl(Fold, {0, []}, B),
  L.

crt([{M, A}|L]) -> crt(A, M, L).

crt(A, _, []) -> A;
crt(A, M, [{MN, AN}|T] = L) ->
  case A rem MN =:= AN of
    true -> crt(A, M * MN, T);
    false -> crt(A + M, M, L)
  end.
