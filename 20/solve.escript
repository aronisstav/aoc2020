#!/usr/bin/env escript
-mode(compile).

%% https://adventofcode.com/2020/day/20

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
  read_tiles(#{}).

read_tiles(Acc) ->
  case io:fread("", "Tile ~d:") of
    eof -> Acc;
    {ok, [D]} ->
      Tile = read_tile({-1, -1, #{}}),
      read_tiles(Acc#{D => Tile})
  end.

read_tile({X, _, T} = Acc) ->
  case io:get_line("") of
    eof -> Acc;
    "\n" -> Acc;
    Line ->
      Fold = fun(C, {I, M}) -> {I + 1, M#{{X + 1, I + 1} => C}} end,
      {L, NM} = lists:foldl(Fold, {-1, T}, Line),
      read_tile({X + 1, L - 1, NM})
  end.

maybe_print(Tiles) ->
  case os:getenv("PRINT") =:= false of
    true -> ok;
    false ->
      MapFold =
        fun(K, {XL, YL, G}, _) ->
            io:format("Tile ~w:~n", [K]),
            ForeachY =
              fun(Y) ->
                  ForeachX =
                    fun(X) ->
                        C = maps:get({Y, X}, G),
                        io:format("~c", [C])
                    end,
                  lists:foreach(ForeachX, lists:seq(0, XL)),
                  io:format("~n")
              end,
            lists:foreach(ForeachY, lists:seq(0, YL)),
            io:format("~n")
        end,
      maps:fold(MapFold, ok, Tiles)
  end.

%%--------------------------------------------------------------------
%% Idea: calculate signatures from each tile's borders, and use them
%%       to match them
%% Sigh: which variable is x and which is y is a mess...
first(Input) ->
  {T1, {Sigs, TIndex}} = make_sigs(Input),
  Arrange = arrange(T1, Sigs, TIndex),
  corner_mul(Arrange).

make_sigs(Tiles) ->
  %% Fix an arbitrary tile K as reference for the rest.
  {K, V, NI} = maps:next(maps:iterator(Tiles)),
  Os = gen_syms([e], get_base_lines(V)),
  {Sigs, TIndex} = add_sigs(K, Os, #{}, #{}),
  {K, make_sigs(NI, {Sigs, TIndex})}.

get_base_lines({X, Y, M}) ->
  T = [maps:get({0, I}, M) || I <- lists:seq(0, X)],
  B = [maps:get({Y, I}, M) || I <- lists:seq(0, X)],
  L = [maps:get({I, 0}, M) || I <- lists:seq(0, Y)],
  R = [maps:get({I, X}, M) || I <- lists:seq(0, Y)],
  [{t, T}, {l, L}, {r, R}, {b, B}].

add_sigs(K, Os, Sigs, TIndex) ->
  Fold =
    fun(O, S, {Acc, TEntry}) ->
        Nums = [{P, num(L)} || {P, L} <- S],
        {add_sigs(Nums, {K, O}, Acc), TEntry#{O => Nums}}
    end,
  {NSigs, TEntry} = maps:fold(Fold, {Sigs, #{}}, Os),
  {NSigs, TIndex#{K => TEntry}}.

num(S) ->
  Fold =
    fun($., Acc) -> 2 * Acc;
       ($#, Acc) -> 2 * Acc + 1
    end,
  lists:foldr(Fold, 0, S).

add_sigs(Nums, {K, O}, Sigs) ->
  Fold =
    fun({P, N}, S) ->
        MM = #{P => #{K => O}},
        Update =
          fun(#{P := M} = V) -> V#{P => M#{K => O}};
             (V) -> V#{P => #{K => O}}
          end,
        maps:update_with(N, Update, MM, S)
    end,
  lists:foldl(Fold, Sigs, Nums).

make_sigs(I, {Sigs, TIndex}) ->
  case maps:next(I) of
    none -> {Sigs, TIndex};
    {K, V, NI} ->
      Syms = [e, r90, r180, r270, m0, m45, m90, m135],
      Os = gen_syms(Syms, get_base_lines(V)),
      make_sigs(NI, add_sigs(K, Os, Sigs, TIndex))
  end.

gen_syms(Syms, BaseLines) ->
  Fold = fun(S, Acc) -> Acc#{S => gen_sym(S, BaseLines)} end,
  lists:foldl(Fold, #{}, Syms).

%% Symmetries for the borders
gen_sym(e, BaseLines) -> BaseLines;
gen_sym(r90, [{t, T}, {l, L}, {r, R}, {b, B}]) ->
  [{t, rev(L)}, {l, B}, {r, T}, {b, rev(R)}];
gen_sym(r180, BL) -> gen_sym(r90, gen_sym(r90, BL));
gen_sym(r270, BL) -> gen_sym(r90, gen_sym(r180, BL));
gen_sym(m0, [{t, T}, {l, L}, {r, R}, {b, B}]) ->
  [{t, rev(T)}, {l, R}, {r, L}, {b, rev(B)}];
gen_sym(m45, BL) -> gen_sym(r90, gen_sym(m0, BL));
gen_sym(m90, BL) -> gen_sym(r180, gen_sym(m0, BL));
gen_sym(m135, BL) -> gen_sym(r270, gen_sym(m0, BL)).

rev(X) -> lists:reverse(X).

arrange(T1, Sigs, TIndex) ->
  Place = {T1, e, {0, 0}},
  Map = {{0, 0}, {0, 0}, #{}},
  arrange(queue:from_list([Place]), Sigs, TIndex, Map).

arrange(Queue, Sigs, TIndex, {XR, YR, M} = Map) ->
  {V, NQ} = queue:out(Queue),
  case V of
    empty -> Map;
    {value, {Tile, O, {X, Y} = Pos}} ->
      case maps:get(Pos, M, none) of
        none ->
          #{Tile := #{O := Nums}} = TIndex,
          FQ = add_neighs(Nums, Tile, Pos, Sigs, NQ),
          NXR = range(X, XR),
          NYR = range(Y, YR),
          NM = M#{Pos => {Tile, O}},
          arrange(FQ, Sigs, TIndex, {NXR, NYR, NM});
        {Tile, O} ->
          arrange(NQ, Sigs, TIndex, Map)
      end
  end.

add_neighs(Nums, Tile, Pos, Sigs, Q) ->
  Fold =
    fun({P, Sig}, Acc) ->
        case find_neighbour(Sig, P, Tile, Pos, Sigs) of
          none -> Acc;
          {true, R} -> queue:in(R, Acc)
        end
    end,
  lists:foldl(Fold, Q, Nums).

find_neighbour(Sig, P, Tile, Pos, Sigs) ->
  Side = n(P),
  #{Sig := Matches} = Sigs,
  case Matches of
    #{Side := Tiles} ->
      case maps:to_list(maps:remove(Tile, Tiles)) of
        [] -> none;
        [{NT, O}] ->
          L = loc(P, Pos),
          {true, {NT, O, L}}
      end;
    _ -> none
  end.

n(t) -> b;
n(l) -> r;
n(r) -> l;
n(b) -> t.

loc(t, {X, Y}) -> {X, Y - 1};
loc(l, {X, Y}) -> {X - 1, Y};
loc(r, {X, Y}) -> {X + 1, Y};
loc(b, {X, Y}) -> {X, Y + 1}.

range(X, {XL, XH}) -> {min(X, XL), max(X, XH)}.

corner_mul({{XL, XH}, {YL, YH}, Arrange}) ->
   #{ {XL, YL} := {TL, _}
    , {XL, YH} := {BL, _}
    , {XH, YL} := {TR, _}
    , {XH, YH} := {BR, _}
    } = Arrange,
   TL * BL * TR * BR.

%%--------------------------------------------------------------------
%% Idea: just do the damn thing (combine images, make mask, scan on
%%       rotations until at least one monster is found).
second(Input) ->
  maybe_print(Input),
  {T1, {Sigs, TIndex}} = make_sigs(Input),
  Arrange = arrange(T1, Sigs, TIndex),
  FinalMap = combine(Arrange, Input),
  find_monsters(FinalMap).

combine({{XL, XH}, {YL, YH}, AMap}, Tiles) ->
  FoldX =
    fun(X, {XP, _, XAcc}) ->
        FoldY =
          fun(Y, {_, YP, YAcc}) ->
              #{{X, Y} := {Tile, O}} = AMap,
              #{Tile := Map} = Tiles,
              {XT, YT, _} = Trimmed = trim(O, Map),
              {XP + XT + 1, YP + YT + 1, paste(Trimmed, XP, YP, YAcc)}
          end,
        lists:foldl(FoldY, {XP, 0, XAcc}, lists:seq(YL, YH))
    end,
  {X, Y, M} = lists:foldl(FoldX, {0, 0, #{}}, lists:seq(XL, XH)),
  {X - 1, Y - 1, M}.

trim(O, Map) ->
  M = map_sym(O, Map),
  remove_border(M).

%% Symmetries for the tiles
map_sym(e, Map) -> Map;
map_sym(r90, {XL, YL, Map}) ->
  FoldX =
    fun(X, XAcc) ->
        FoldY =
          fun(Y, YAcc) ->
              #{{Y, X} := C} = Map,
              YAcc#{{X, YL - Y} => C}
          end,
        lists:foldl(FoldY, XAcc, lists:seq(0, YL))
    end,
  {XL, YL, lists:foldl(FoldX, #{}, lists:seq(0, XL))};
map_sym(r180, Map) -> map_sym(r90, map_sym(r90, Map));
map_sym(r270, Map) -> map_sym(r180, map_sym(r90, Map));
map_sym(m0, {XL, YL, Map}) ->
  FoldX =
    fun(X, XAcc) ->
        FoldY =
          fun(Y, YAcc) ->
              #{{Y, X} := C} = Map,
              YAcc#{{Y, XL - X} => C}
          end,
        lists:foldl(FoldY, XAcc, lists:seq(0, YL))
    end,
  {XL, YL, lists:foldl(FoldX, #{}, lists:seq(0, XL))};
map_sym(m45, BL) -> map_sym(r90, map_sym(m0, BL));
map_sym(m90, BL) -> map_sym(r180, map_sym(m0, BL));
map_sym(m135, BL) -> map_sym(r270, map_sym(m0, BL)).

remove_border({XL, YL, Map}) ->
  FoldX =
    fun(X, XAcc) ->
        FoldY =
          fun(Y, YAcc) ->
              #{{Y + 1, X + 1} := C} = Map,
              YAcc#{{Y, X} => C}
          end,
        lists:foldl(FoldY, XAcc, lists:seq(0, YL - 2))
    end,
  {XL - 2, YL - 2, lists:foldl(FoldX, #{}, lists:seq(0, XL - 2))}.

paste({XS, YS, Map}, XO, YO, Acc) ->
  FoldX =
    fun(X, XAcc) ->
        FoldY =
          fun(Y, YAcc) ->
              #{{Y, X} := C} = Map,
              YAcc#{{Y + YO, X + XO} => C}
          end,
        lists:foldl(FoldY, XAcc, lists:seq(0, YS))
    end,
  lists:foldl(FoldX, Acc, lists:seq(0, XS)).

find_monsters(FinalMap) ->
  maybe_print(#{combined => FinalMap}),
  Pattern = pattern(),
  Syms = [e, r90, r180, r270, m0, m45, m90, m135],
  search(Syms, Pattern, FinalMap).

pattern() ->
  Pattern =
    [ "                  # "
    , "#    ##    ##    ###"
    , " #  #  #  #  #  #   "
    ],
  convert(Pattern).

convert(Pattern) ->
  FoldY =
    fun(L, {Y, YAcc}) ->
        FoldX = fun($#, {X, XAcc}) -> {X + 1, [{Y, X}|XAcc]};
                   ( _, {X, XAcc}) -> {X + 1, XAcc}
                end,
        {_, Xs} = lists:foldl(FoldX, {0, YAcc}, L),
        {Y + 1, Xs}
    end,
  {_, L} = lists:foldl(FoldY, {0, []}, Pattern),
  L.

search([], _, _) -> -1;
search([O|R], Pattern, {X, Y, _} = Map) ->
  Flipped = map_sym(O, Map),
  {Mon, NMap} = search(Pattern, Flipped),
  case Mon =:= 0 of
    true -> search(R, Pattern, Map);
    false ->
      maybe_print(#{final => {X, Y, NMap}}),
      Fold =
        fun(_, $#, C) -> C + 1;
           (_,  _, C) -> C
        end,
      io:format("Found ~p monsters!~n", [Mon]),
      maps:fold(Fold, 0, NMap)
  end.

search(Pattern, Map) ->
  search(0, 0, Pattern, Map, 0).

search(YO, _XO, _Pattern, {_XM, YM, Map}, Acc) when YO > YM -> {Acc, Map};
search(YO, XO, Pattern, {XM, YM, Map}, Acc) when XO > XM ->
  search(YO + 1, 0, Pattern, {XM, YM, Map}, Acc);
search(YO, XO, Pattern, {XM, YM, Map}, Acc) ->
  NP = [{Y + YO, X + XO} || {Y, X} <- Pattern],
  {NMap, NAcc} =
    case scan(NP, Map) of
      {true, NM} -> {NM, Acc + 1};
      false -> {Map, Acc}
    end,
  search(YO, XO + 1, Pattern, {XM, YM, NMap}, NAcc).

scan(Pattern, Map) ->
  Fold =
    fun(_, false) -> false;
       (P, Acc) ->
        case maps:get(P, Acc, $X) of
          $X -> false;
          $. -> false;
          _ -> Acc#{P => $O}
        end
    end,
  case lists:foldl(Fold, Map, Pattern) of
    false -> false;
    Other -> {true, Other}
  end.
