-module(vm).

-export([ load/1
        , remake/0
        , run/2
        ]).

%%--------------------------------------------------------------------
load(File) ->
  {ok, Bin} = file:read_file(File),
  Str = binary_to_list(Bin),
  load_code(Str).

load_code(Str) ->
  load_code(Str, #{}, 0).

load_code(Str, Mem, Addr) ->
  case io_lib:fread("~a ~d", Str) of
    {ok, [Op, Arg], R} ->
      load_code(R, Mem#{Addr => {Op, Arg}}, Addr + 1);
    _ ->
      maybe_dump(Mem),
      Mem
  end.

%%--------------------------------------------------------------------
remake() ->
  {ok, CWD} = file:get_cwd(),
  file:set_cwd(filename:join([CWD, "..", "vm"])),
  TmpDir = "tmp",
  ok = filelib:ensure_dir(filename:join([TmpDir,"file"])),
  MakeOpts =
    [ debug_info
    , {outdir, TmpDir}
    ],
  up_to_date = make:all([load, {emake, [{'*', MakeOpts}]}]),
  {ok, Files} = file:list_dir(TmpDir),
  [ok = file:rename(filename:join([TmpDir, F]), F) || F <- Files],
  ok = file:del_dir(TmpDir),
  ok = file:set_cwd(CWD).

%%--------------------------------------------------------------------
run(Code, Opts) ->
  Init = init(Code),
  run(Code, Init, Opts).

run(Code, State, Opts) ->
  Continue =
    check_run_once(State, Opts) andalso
    check_cp_bound(State),
  case Continue of
    false -> State;
    true ->
      #{ acc   := Acc
       , cp    := CP
       , execd := Execd
       } = State,
      #{CP := Op} = Code,
      BaseNewState = State#{execd => Execd#{CP => true}, cp => CP + 1},
      NewState =
        case Op of
          {nop, _} -> BaseNewState;
          {acc, N} -> BaseNewState#{acc => Acc + N};
          {jmp, N} -> BaseNewState#{cp => CP + N}
        end,
      run(Code, NewState, Opts)
  end.

check_cp_bound(#{cp := CP, size := Size}) -> CP < Size.
   
check_run_once(State, #{run_once := true}) ->
  #{ cp := CP
   , execd := Execd
   } = State,
  case Execd of
    #{CP := _} -> false;
    _ -> true
  end;
check_run_once(_, _) -> true.            

%%--------------------------------------------------------------------
init(Code) ->
  #{ acc => 0
   , cp => 0
   , execd => #{}
   , size => maps:size(Code)
   }.

maybe_dump(Code) ->
  case os:getenv("DUMP") of
    false -> ok;
    _ -> stderr("DUMP:~n  ~p~n~n", [Code])
  end.
      
stderr(Fmt, Args) ->
  io:format(standard_error, Fmt, Args).
