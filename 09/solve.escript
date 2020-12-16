#!/usr/bin/env escript
-mode(compile).

%% https://adventofcode.com/2020/day/9

main([SizeStr|Args]) ->
  Size = list_to_integer(SizeStr),
  Input = read_list("~d"),
  Ans =
    case Args =:= ["2"] of
      false -> first(Size, Input);
      true -> second(Size, Input)
    end,
  io:format("~w~n", [Ans]).

%%--------------------------------------------------------------------

read_list(Pat) -> read_list(Pat, []).

read_list(Pat, Acc) ->
  case io:fread("", Pat) of
    {ok, Res} -> read_list(Pat, [Res|Acc]);
    eof ->
      Fold = fun(L, R) -> lists:append(L, R) end,
      lists:foldl(Fold, [], Acc)
  end.

%%--------------------------------------------------------------------

first(Size, Input) ->
  {Preamble, Stream} = lists:split(Size, Input),
  Presum = queue:from_list(Preamble),
  presum(Stream, Presum).

second(Size, Input) ->
  Goal = first(Size, Input),
  findsum(Goal, Input).

%%--------------------------------------------------------------------

presum([], _) -> -1;
presum([N|R], Presum) ->
  L = queue:to_list(Presum),
  case check_presum(N, L) of
    true -> presum(R, queue:in(N, queue:drop(Presum)));
    false -> N      
  end.

check_presum(_, []) -> false;
check_presum(N, [H|R]) -> check_presum(N, H, R) orelse check_presum(N, R).

check_presum(_, _, []) -> false;
check_presum(N, H, [T|_]) when N =:= H + T -> true;
check_presum(N, H, [_|T]) -> check_presum(N, H, T).

findsum(G, L) ->
  findsum(G, L, 0, queue:new()).

findsum(_, [], _, _) -> -1;
findsum(G, [H|T] = L, S, Q) ->
  NS = S + H,
  if NS =:= G ->
      All = [H|queue:to_list(Q)],
      Min = lists:min(All),
      Max = lists:max(All),
      Min + Max;
     NS < G ->
      findsum(G, T, NS, queue:in(H, Q));
     NS > G ->
      {{value, LS}, NQ} = queue:out(Q),
      findsum(G, L, S - LS, NQ)
  end.
