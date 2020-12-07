#!/usr/bin/env escript

%% https://adventofcode.com/2020/day/7

-mode(compile).

main(Args) ->
  Input = read_input(#{}),
  Ans =
    case Args of
      ["2"] -> second(Input);
      _ -> first(Input)
    end,
  io:format("~w~n", [Ans]).

read_input(Acc) ->
  case io:get_line("") of
    eof -> Acc;
    Line ->
      {ok, [D, C], Rest} = io_lib:fread("~a ~a bags contain", Line),
      Contents = read_spec(Rest, #{}),
      read_input(Acc#{{D, C} => Contents})
  end.

read_spec(Line, Acc) ->
  case io_lib:fread(" ~d ~a ~a ~s", Line) of
    {ok, [N, D, C, _], Rest} -> read_spec(Rest, Acc#{{D, C} => N});
    _ -> Acc
  end.

first(Input) ->
  G = digraph:new(),
  Fold = 
    fun(K, M, _) ->
        InnerFold =
          fun(C, _, _) ->
              digraph:add_vertex(G, C),
              digraph:add_vertex(G, K),
              digraph:add_edge(G, C, K)
          end,
        maps:fold(InnerFold, ignore, M)
    end,
  maps:fold(Fold, ignore, Input),
  P = digraph_utils:reachable([{shiny, gold}], G),
  length(P) - 1.

second(Input) ->
  {V, _} = value({shiny, gold}, Input, #{}),
  V - 1.

value(X, Input, C) ->
  case C of
    #{X := V} -> {V, C};
    _ ->
      #{X := M} = Input,
      Fold = 
        fun(K, N, {VAcc, CAcc}) ->
            {V, NAcc} = value(K, Input, CAcc),
            {VAcc + N * V, NAcc}
        end,
      maps:fold(Fold, {1, C}, M)
  end.
