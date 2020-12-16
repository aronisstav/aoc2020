#!/usr/bin/env escript
-mode(compile).

%% https://adventofcode.com/2020/day/16

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
  Ranges = read_ranges(#{}),
  {ok, []} = io:fread("", "your ticket:"),
  Line = io:get_line(""),
  Mine = parse_ticket(Line),
  {ok, []} = io:fread("", "nearby tickets:"),
  Others = read_tickets([]),
  {Ranges, Mine, Others}.

read_ranges(Fields) ->
  case io:get_line("") of
    "\n" -> Fields;
    FieldDesc ->
      [NameStr, Rest] = string:split(FieldDesc, ":"),
      Name = list_to_atom(NameStr),
      {ok, [Min1, Max1, Min2, Max2], _} = io_lib:fread("~d-~d or ~d-~d\n", Rest),
      read_ranges(Fields#{Name => [{Min1, Max1}, {Min2, Max2}]})
  end.

read_tickets(Acc) ->
  case io:get_line("") of
    eof -> Acc;
    Line -> read_tickets([parse_ticket(Line)|Acc])
  end.

parse_ticket(Line) ->
  L = [C || C <- Line, C =/= $\n],
  [list_to_integer(I) || I <- string:split(L, ",", all)].

%%--------------------------------------------------------------------
first(Input) ->
  {Ranges, _, Others} = Input,
  Fold =
    fun(T, Acc) ->
        FoldI =
          fun(V, AccI) ->
              case valid(V, Ranges) of
                true -> AccI;
                false -> AccI + V
              end
          end,
        lists:foldl(FoldI, Acc, T)
    end,
  lists:foldl(Fold, 0, Others).

valid(V, Ranges) ->
  Fold = fun(_, Vals, Acc) -> Acc orelse within(V, Vals) end,
  maps:fold(Fold, false, Ranges).

within(V, [{Min1, Max1}, {Min2, Max2}]) ->
  (Min1 =< V andalso V =< Max1) orelse (Min2 =< V andalso V =< Max2).

%%--------------------------------------------------------------------
second(Input) ->
  {Ranges, Mine, Others} = Input,
  case find_mapping(Others, Ranges) of
    {ok, Mapping} ->
      Fs = dep_fields(Mapping, Mine),
      lists:foldl(fun(X, A) -> X * A end, 1, Fs);
    Else -> Else
  end.

find_mapping(Tickets, Ranges) ->
  Filter = fun(T) -> lists:all(fun(X) -> valid(X, Ranges) end, T) end,
  ValidTickets = [T || T <- Tickets, Filter(T)],
  L = maps:size(Ranges),
  Options = lists:seq(1, L),
  Map = fun(_, _) -> Options end,
  Init = maps:map(Map, Ranges),
  Mapping = find_mapping(ValidTickets, Ranges, Init),
  Fold =
    fun(K, [O], {Complete, Acc}) -> {Complete, Acc#{K => O}};
       (K,  Os, {_, Acc}) -> {incomplete, Acc#{K => Os}}
    end,
  maps:fold(Fold, {ok, #{}}, Mapping).

find_mapping(ValidTickets, Ranges, Mapping) ->
  Fold = fun(T, Acc) -> update_mapping(T, 1, Ranges, Acc) end,
  M1 = lists:foldl(Fold, Mapping, ValidTickets),
  eliminate(M1).

update_mapping([], _, _, Mapping) -> Mapping;
update_mapping([H|T], N, Ranges, Mapping) ->
  Fold =
    fun(K, R, Acc) ->
        #{K := Vals} = Ranges,
        case within(H, Vals) of
          true -> Acc#{K => R};
          false -> Acc#{K => R -- [N]}
        end
    end,
  update_mapping(T, N + 1, Ranges, maps:fold(Fold, #{}, Mapping)).

dep_fields(Mapping, Ticket) ->
  Fold =
    fun(K, V, Acc) ->
        case atom_to_list(K) of
          "departure" ++ _ -> [lists:nth(V, Ticket)|Acc];
          _ -> Acc
        end
    end,
  maps:fold(Fold, [], Mapping).

eliminate(Mapping) ->
  Fold =
    fun(K1, [V], {Acc, M1}) ->
        FoldI =
          fun(K2, Vs, M2) when K2 =:= K1 -> M2#{K2 => Vs};
             (K2, Vs, M2) -> M2#{K2 => Vs -- [V]}
          end,
        NM1 = maps:fold(FoldI, #{}, M1),
        {Acc orelse NM1 =/= M1, NM1};
       (_, _, Acc) -> Acc
    end,
  case maps:fold(Fold, {false, Mapping}, Mapping) of
    {true, New} -> eliminate(New);
    {false, Mapping} -> Mapping
  end.
