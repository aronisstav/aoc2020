#!/usr/bin/env escript
-mode(compile).

%% https://adventofcode.com/2020/day/19

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
  Lines = read_lines([]),
  parse(Lines).

read_lines(Acc) ->
  case io:get_line("") of
    eof ->
      Fold = fun(S, R) -> [S|R] end,
      lists:foldl(Fold, [], Acc);
    Res -> read_lines([Res|Acc])
  end.

parse(Input) ->
  {Rules, Rest} = parse_rules(Input, []),
  Inputs = parse_input(Rest),
  {prep(Rules), Inputs}.

parse_rules(["\n"|R], Acc) -> {maps:from_list(Acc), R};
parse_rules([H|T], Acc) ->
  {ok, [N], R} = io_lib:fread("~d: ", H),
  [R1, []] = string:split(R, "\n"),
  PatsS = string:split(R1, " ", all),
  Pats = [form(P) || P <- PatsS],
  parse_rules(T, [{N, Pats}|Acc]).

form("|") -> '|';
form("\"" ++ R) ->
  [C|_] = R,
  {c, C};
form(N) -> list_to_integer(N).

prep(Rules) ->
  Map =
    fun(_, [{c, L}]) -> {c, L};
       (_, Else) -> {r, r(Else, [], [])}
    end,
  maps:map(Map, Rules).

r([], L, Acc) -> [lists:reverse(L)|Acc];
r(['|'|T], L, Acc) -> r(T, [], [lists:reverse(L)|Acc]);
r([N|T], L, Acc) -> r(T, [N|L], Acc).

parse_input(Input) ->
  Map =
    fun(L) ->
        [R, []] = string:split(L, "\n"),
        R
    end,
  [Map(L) || L <- Input].  

%%--------------------------------------------------------------------
first({Rules, Input}) ->
  RegExp = build(0, Rules),
  matching("^" ++ RegExp ++ "$", Input).

build(N, Rules) ->
  #{N := R} = Rules,
  compose(R, Rules).

compose({c, L}, _Rules) -> [L];
compose({r, [A]}, Rules) -> t(A, Rules);
compose({r, Alts}, Rules) ->
  "(" ++ string:join([t(R, Rules) || R <- Alts], "|") ++ ")".

t(L, Rules) ->
  lists:append([compose(maps:get(N, Rules), Rules) || N <- L]).

matching(RE, Inputs) ->
  Map = [{I, re:run(I, RE)} || I <- Inputs],
  length([I || {I, R} <- Map, R =/= nomatch]).

%%--------------------------------------------------------------------
%% We take advantage of the fact that:
%%  0: 8 11
%%  8: 42 | 42 8
%% 11: 42 31 | 42 11 31
%% This means that the final pattern is:
%% * At least one but maybe more 'unmatched' 42, followed by
%% * At least one but maybe more 42, each matched by a later 31
second({Rules, Input}) ->
  One = "^" ++ build(42, Rules),
  Two = "^" ++ build(31, Rules),
  Map = [{I, matching_2(I, One, Two, [])} || I <- Input],
  length([I || {I, M} <- Map, is_ok(M)]).                

matching_2([], _, _, Acc) -> lists:reverse(Acc);
matching_2(Input, One, Two, Acc) ->
  Opts = [{capture, first, list}],
  case re:run(Input, One, Opts) of
    nomatch ->
      case re:run(Input, Two, Opts) of
        nomatch -> fail;
        {match, [C]} ->
          Rest = Input -- C,
          matching_2(Rest, One, Two, [2|Acc])
      end;
    {match, [C]} ->
      Rest = Input -- C,
      matching_2(Rest, One, Two, [1|Acc])
  end.

is_ok([1|R]) -> is_ok(R, true, 1);
is_ok(_) -> false.

is_ok([], false, N) when N > 0 -> true;
is_ok([1|R], true, N) -> is_ok(R, true, N + 1);
is_ok([2|R], _, N) when N > 0 -> is_ok(R, false, N - 1);
is_ok(_, _, _) -> false.
