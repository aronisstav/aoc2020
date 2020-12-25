#!/usr/bin/env escript
-mode(compile).

%% https://adventofcode.com/2020/day/18

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
  case io:get_line("") of
    eof -> lists:reverse(Acc);
    Res -> read_list(Pat, [scan(Res)|Acc])
  end.

scan(L) ->
  L1 = string:replace(L, "(", "( ", all),
  L2 = string:replace(L1, ")", " )", all),
  [Cs,[]] = string:split(L2, "\n"),
  Tokens = string:split(Cs, " ", all),
  lists:foldr(fun scan/2, [], Tokens).

scan("\n", Acc) -> Acc;
scan("(", Acc) -> ['('|Acc];
scan(")", Acc) -> [')'|Acc];
scan("+", Acc) -> ['+'|Acc];
scan("*", Acc) -> ['*'|Acc];
scan(D, Acc) -> [list_to_integer(D)|Acc].

%%--------------------------------------------------------------------
first(Input) ->
  lists:sum([eval(L) || L <- Input]).

eval([V]) -> V;
eval(['('|R]) ->
  {V, NR} = eval_p(R),
  eval([V|NR]);
eval([N,'+'|R]) ->
  {V, NR} = eval_one(R),
  eval([N + V|NR]);
eval([N,'*'|R]) ->
  {V, NR} = eval_one(R),
  eval([N * V|NR]).

eval_one(['('|R]) ->
  eval_p(R);
eval_one([D|R]) ->
  {D, R}.

eval_p(S) ->
  {P, R} = eval_p(S , []),
  {eval(P), R}.

eval_p([')'|R], Acc) ->
  {lists:reverse(Acc), R};
eval_p(['('|R], Acc) ->
  {V, NR} = eval_p(R),
  eval_p(NR, [V|Acc]);
eval_p([S|R], Acc) -> eval_p(R, [S|Acc]).

%%--------------------------------------------------------------------
second(Input) ->
  lists:sum([calc2(L) || L <- Input]).

calc2(L) ->
  io:format("~p~n", [L]),
  R = eval2(L),
  io:format("~p~n",[R]),
  R.

eval2([V]) -> V;
eval2(['('|R]) ->
  {V, NR} = eval_p2(R),
  eval2([V|NR]);
eval2([N,'+'|R]) ->
  {V, NR} = eval_one2(R),
  eval2([N + V|NR]);
eval2([N,'*'|R]) ->
  {V, NR} = eval_mul(R),
  eval2([N * V|NR]).

eval_one2(['('|R]) ->
  eval_p2(R);
eval_one2([D|R]) ->
  {D, R}.

eval_mul(['('|R]) ->
  {V, NR} = eval_p2(R),
  eval_mul([V|NR]);
eval_mul([N,'+'|R]) ->
  {V, NR} = eval_one2(R),
  eval_mul([N + V|NR]);
eval_mul([N|R]) ->
  {N, R}.

eval_p2(S) ->
  {P, R} = eval_p2(S , []),
  {eval2(P), R}.

eval_p2([')'|R], Acc) ->
  {lists:reverse(Acc), R};
eval_p2(['('|R], Acc) ->
  {V, NR} = eval_p2(R),
  eval_p2(NR, [V|Acc]);
eval_p2([S|R], Acc) -> eval_p2(R, [S|Acc]).
