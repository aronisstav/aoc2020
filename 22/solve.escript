#!/usr/bin/env escript
-mode(compile).

%% https://adventofcode.com/2020/day/22

main(Args) ->
  Input = read_input(),
  Ans =
    case Args =:= ["2"] of
      false -> first(Input);
      true -> second(Input)
    end,
  io:format("~p~n", [Ans]).

%%--------------------------------------------------------------------
read_input() ->
  {read_player(), read_player()}.

read_player() ->
  _ = io:fread("", "Player ~d:"),
  read_line_nums([]).

read_line_nums(Acc) ->
  case io:get_line("") of
    eof -> lists:reverse(Acc);
    "\n" -> lists:reverse(Acc);
    Line ->
      {ok, [D], _} = io_lib:fread("~d", Line),
      read_line_nums([D|Acc])
  end.

%%--------------------------------------------------------------------
first({P1, P2}) ->
  [D1, D2] = [queue:from_list(D) || D <- [P1, P2]],
  {2, D} = play(D1, D2),
  score(D).

play(P1, P2) ->
  {C1, NQ1} = queue:out(P1),
  {C2, NQ2} = queue:out(P2),
  case {C1, C2} of
    {empty, _} -> {2, queue:to_list(P2)};
    {_, empty} -> {1, queue:to_list(P1)};
    {{value, V1}, {value, V2}} ->
      {FQ1, FQ2} =
        case V1 > V2 of
          true -> {queue:in(V2, queue:in(V1, NQ1)), NQ2};
          false -> {NQ1, queue:in(V1, queue:in(V2, NQ2))}
        end,
      play(FQ1, FQ2)
  end.

score(D) ->
  Fold = fun(V, {N, Acc}) -> {N + 1, Acc + N * V} end,
  lists:foldr(Fold, {1, 0}, D).

%%--------------------------------------------------------------------
second({P1, P2}) ->
  {1, D} = playr(P1, P2, #{}),
  score(D).

playr([], P2, _) -> {2, P2};
playr(P1, [], _) -> {1, P1};
playr([H1|R1] = P1, [H2|R2] = P2, M) ->
  case M of
    #{{P1, P2} := true} -> {1, -1};
    _ ->
      L1 = length(R1),
      L2 = length(R2),
      Winner =
        case (H1 =< L1) andalso (H2 =< L2) of
          true ->
            {W, _} = playr(sub(R1, H1), sub(R2, H2), #{}),
            W;
          false ->
            case H1 > H2 of
              true -> 1;
              false -> 2
            end
        end,
      {NP1, NP2} =
        case Winner of
          1 -> {R1 ++ [H1, H2], R2};
          2 -> {R1, R2 ++ [H2, H1]}
        end,
      NM = M#{{P1, P2} => true},
      playr(NP1, NP2, NM)
  end.

sub(L, N) -> lists:sublist(L, N).
