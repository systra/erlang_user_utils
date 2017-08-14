-module(user_default).

% Usage:
% Compile and add the following to ~/.erlang to enable:
%
% code:load_abs("/Path/To/erlang_user_funs/user_default").

-export([help/0,
         dbg/1, dbg/2, dbg/3, dbg/4, dbgo/0, dbgt/1,
         p/1, dmfa/3,
         l/0, nl/0, mm/0,
         mk/0,
         decompile/1, reloader/0,
         hexstr2bin/1, bin2hexstr/1,
         tc/2, tc/4, tca/2, tca/4]).

-import(io, [format/1, format/2]).

-compile(inline).

help() ->
    shell_default:help(),
    format("~n** commands from module ~s (user helpers) **~n~n", [?MODULE]),
    format("dbg(c)          -- clear all traces = stop tracing\n"),
    format("dbg(M)          -- trace on module M\n"),
    format("dbg(c, T)       -- schedule clearing all traces after T msec\n"),
    format("dbg(M, O)       -- trace on module M with options O\n"),
    format("dbg(M, F)       -- trace on function M:F\n"),
    format("dbg(M, F, O)    -- trace on function M:F with options O\n"),
    format("dbg(M, F, A)    -- trace on function M:F(A)\n"),
    format("dbg(M, F, A, O) -- trace on function M:F(A) with options O\n"),
    format("dbgo()          -- print debugging options help\n"),
    format("dbgt(File)      -- print trace data read from File\n"),
    format("p(Term)         -- print term using io:format(\"\~s\", [Term])\n",["~p~n"]),
    format("l()             -- load all changed modules\n"),
    format("nl()            -- load all changed modules on all known nodes\n"),
    format("mm()            -- list modified modules\n"),
    format("mk()            -- compile all modules specified in Emakefile or current dir\n"),
    format("dmfa(M, F, A)   -- run M:F(A) on all visible nodes\n"),
    format("tc(N, M, F, A)  -- evaluate M:F(A) N times\n"),
    format("tc(N, F)        -- evaluate F N times\n"),
    format("tca(N, M, F, A) -- evaluate M:F(A) N times and calculate stats\n"),
    format("tca(N, F)       -- evaluate F N times and calculate stats\n"),
    format("hexstr2bin(Hex) -- converts HEX string to binary\n"),
    format("bin2hexstr(Bin) -- converts binary to HEX string\n"),
    format("decompile(Beam) -- print source code of the file beam (if available)\n"),
    format("reloader()      -- run code reloader\n"),
    ok.

dbgo() ->
    format("Debugging options:\n"),
    format("c  -- clear\n"),
    format("r  -- show return trace (incl. exceptions)\n"),
    format("l  -- trace on local functions\n"),
    format("lr -- trace on local functions with return trace (incl. exceptions)\n"),
    ok.

dbgt(File) ->
    Fun = fun({trace, _, call, {M, F, A}}, _) -> io:format(user, "call: ~w:~w~w~n", [M,F,A]);
             ({trace, _, return_from, {M, F, A}, R}, _) -> io:format(user, "retn: ~w:~w/~w -> ~w~n", [M,F,A,R]);
             (A,B) -> io:format(user, "~w: ~w~n", [A, B]) end,
    dbg:trace_client(file, File, {Fun, []}).

%--- Code Reload

l() ->
    lists:foreach(
      fun(M) ->
              io:format(user, "Loading ~p -> ~p~n", [M, c:l(M)])
      end,
      mm()
     ).

nl() ->
    lists:foreach(
      fun(M) ->
              io:format(user, "Network loading ~p -> ~p~n", [M, c:nl(M)])
      end,
      mm()
     ).

mm() ->
    modified_modules().

mk() ->
    up_to_date = make:all([load]).

%--- Benchmarking -------------------------------------------------------------

tc(N, F) when N > 0, is_function(F) ->
    time_it(fun() -> exit(call(N, N, F, erlang:now())) end).

tc(N, M, F, A) when N > 0, is_atom(M), is_atom(F), is_list(A) ->
    time_it(fun() -> exit(call(N, N, M, F, A, erlang:now())) end).

tca(N, F) when N > 0, is_function(F) ->
    print_tca_result(tc_loop(F, N, [])).

tca(N, M, F, A) when N > 0, is_atom(M), is_atom(F), is_list(A) ->
    print_tca_result(tc_loop(M, F, A, N, [])).

print_tca_result(L) ->
    % L0 = tl(lists:reverse(L)),
    case code:ensure_loaded(bear) of
        {module, bear} ->
            % use bear for stats
            bear:get_statistics(L);
        _ ->
            % calc stats manually
            Length = length(L),
            Min = lists:min(L),
            Max = lists:max(L),
            Med = lists:nth(round((Length / 2)), lists:sort(L)),
            Avg = lists:foldl(fun(X, Sum) -> X + Sum end, 0, L) / Length,
            [{min, Min},
             {max, Max},
             {arithmetic_mean, Avg},
             {median, Med},
             {n, Length}]
    end.

%--- Debugging

% Modified version of: https://github.com/eproxus/erlang_user_utilities/blob/master/user_default.erl

dbg(t) -> start_tracer(test);
dbg(f) -> start_tracer(flat);
dbg(n) -> start_tracer(nested);
dbg(c) -> dbg:stop_clear();
dbg(M) -> dbge({M, '_', '_'}, []).

dbg(x, File) -> {ok, _} = dbg:tracer(port, dbg:trace_port(file, File));
dbg(c, MSec) when is_number(MSec) ->
    spawn(fun() ->
                  timer:sleep(MSec),
                  case dbg:get_tracer() of
                      {ok, _} ->
                          dbg:stop_clear(),
                          io:format(user, "debugging has been disabled~n", []);
                      _ -> ok
                  end
          end),
    ok;
dbg(M, c) -> dbgc({M, '_', '_'});
dbg(M, r) -> dbge({M, '_', '_'}, dbg_rt());
dbg(M, l) -> dbgl({M, '_', '_'}, []);
dbg(M, lr) -> dbgl({M, '_', '_'}, dbg_rt());
dbg(M, F) when is_atom(F) -> dbge({M, F, '_'}, []);
dbg(M, O) -> dbge({M, '_', '_'}, O).

dbg(M, F, c) -> dbgc({M, F, '_'});
dbg(M, F, l) -> dbgl({M, F, '_'}, dbg_rt());
dbg(M, F, r) -> dbge({M, F, '_'}, dbg_rt());
dbg(M, F, lr) -> dbgl({M, F, '_'}, dbg_rt());
dbg(M, F, A) when is_integer(A) -> dbge({M, F, A}, []);
dbg(M, F, O) -> dbge({M, F, '_'}, O).

dbg(M, F, A, c) -> dbgc({M, F, A});
dbg(M, F, A, r) -> dbge({M, F, A}, dbg_rt());
dbg(M, F, A, l) -> dbgl({M, F, A}, dbg_rt());
dbg(M, F, A, lr) -> dbgl({M, F, A}, dbg_rt());
dbg(M, F, A, O) -> dbge({M, F, A}, O).

%--- Other

p(Term) ->
    io:format(user, "~p~n", [Term]).

reloader() -> sync:go().

decompile(Beam) when is_list(Beam); is_atom(Beam) ->
    case get_abstract_code(Beam) of
        {ok, _Module, _Basename, Forms} ->
            io:fwrite("~s~n", [erl_prettypr:format(erl_syntax:form_list(Forms))]);
        {ok, {_, [{abstract_code, no_abstract_code}]}} ->
            io:format(user, "Error: file ~s has no abstract code!\n", [Beam]),
            {error, no_abstract_code};
        Error ->
            Error
    end.

dmfa(M, F, A) ->
    Nodes = nodes(),
    case Nodes of
        [] ->
            apply(M, F, A);
        _ ->
            rpc:multicall(M, F, A)
    end.

%--- Debugging Functions

dbgc(MFA) ->
    dbg:ctp(MFA).

dbge(MFA, O) ->
    start_tracer(),
    dbg:p(all, call),
    dbg:tp(MFA, O).

dbgl(MFA, O) ->
    start_tracer(),
    dbg:p(all, call),
    dbg:tpl(MFA, O).

dbg_rt() -> cx.

start_tracer() ->
    start_tracer(regular).

start_tracer(Type) ->
    case do_start_tracer(Type) of
        {ok, _} -> ok;
        {error, already_started} -> ok;
        E -> E
    end.

do_start_tracer(test) ->
    dbg:tracer(process, {fun test_trace/2, 0});
do_start_tracer(flat) ->
    dbg:tracer(process, {fun flat_trace/2, 0});
do_start_tracer(nested) ->
    dbg:tracer(process, {fun nested_trace/2, 0});
do_start_tracer(_) ->
    dbg:tracer().

test_trace(Trace, Level) ->
    Info = trace_format(Trace),
    io:format(user, "~s", [Info]),
    Level.

flat_trace({trace, Pid, call, {Mod, Fun, Args}}, Level) ->
    io:format(user, "Args: ~p", [Args]),
    % [[91, S, 93]] = io_lib:format("~p", [Args]),
    S = Args,
    SArgs = lists:flatten(S),
    io:format(user, "DBG ~p CALL :: ~p:~p(~s) :: L#~p~n", [Pid, Mod, Fun, SArgs, Level]),
    Level + 1;
flat_trace({trace, Pid, call, {Mod, Fun, Args}, _}, Level) ->
    [[91, S, 93]] = io_lib:format("~p", [Args]),
    SArgs = lists:flatten(S),
    io:format(user, "DBG ~p CALL :: ~p:~p(~s) :: L#~p~n", [Pid, Mod, Fun, SArgs, Level]),
    Level + 1;
flat_trace({trace, Pid, return_from, {_, _, _}, ReturnValue}, Level) ->
    NewLevel = Level - 1,
    io:format(user, "DBG ~p RTRN :: ~p :: L#~p~n", [Pid, ReturnValue, NewLevel]),
    NewLevel;
flat_trace(Trace, Level) ->
    io:format(user, "DBG MSG :: ~p :: L#~p~n", [Trace, Level]),
    Level.

nested_trace({trace, Pid, call, {Mod, Fun, Args}}, Level) ->
    [[91, S, 93]] = io_lib:format("~p", [Args]),
    SArgs = lists:flatten(S),
    io:format(user, "DBG ~p CALL :: ~s~p:~p(~s)~n", [Pid, filler(Level), Mod, Fun, SArgs]),
    Level + 1;
nested_trace({trace, Pid, call, {Mod, Fun, Args}, _}, Level) ->
    [[91, S, 93]] = io_lib:format("~p", [Args]),
    SArgs = lists:flatten(S),
    io:format(user, "DBG ~p CALL :: ~s~p:~p(~s)~n", [Pid, filler(Level), Mod, Fun, SArgs]),
    Level + 1;
nested_trace({trace, Pid, return_from, {_, _, _}, ReturnValue}, Level) ->
    NewLevel = Level - 1,
    io:format(user, "DBG ~p RTRN :: ~s~p~n", [Pid, filler(NewLevel), ReturnValue]),
    NewLevel;
nested_trace(Trace, Level) ->
    io:format(user, "DBG msg :: ~p~n", [Trace]),
    Level.

filler(Level) ->
    string:copies("| ", Level).

time_it(F) ->
    Pid  = spawn_opt(F, [{min_heap_size, 16384}]),
    MRef = erlang:monitor(process, Pid),
    receive
        {'DOWN', MRef, process, _, Result} -> Result
    end.

call(1, X, F, Time1) ->
    Res = (catch F()),
    return(X, Res, Time1, erlang:now());
call(N, X, F, Time1) ->
    (catch F()),
    call(N-1, X, F, Time1).

call(1, X, M, F, A, Time1) ->
    Res = (catch apply(M, F, A)),
    return(X, Res, Time1, erlang:now());
call(N, X, M, F, A, Time1) ->
    catch apply(M, F, A),
    call(N-1, X, M, F, A, Time1).

return(N, Res, Time1, Time2) ->
    Int = timer:now_diff(Time2, Time1),
    [{total, Int},
     {arithmetic_mean, Int / N},
     {last_result, Res}].

modified_modules() ->
    [M || {M, _} <-  code:all_loaded(), module_modified(M) == true].

module_modified(Module) ->
    case code:is_loaded(Module) of
        {file, preloaded} ->
            false;
        {file, Path} ->
            CompileOpts = proplists:get_value(compile, Module:module_info()),
            CompileTime = proplists:get_value(time, CompileOpts),
            Src = proplists:get_value(source, CompileOpts),
            module_modified(Path, CompileTime, Src);
        _ ->
            false
    end.

module_modified(Path, PrevCompileTime, PrevSrc) ->
    case find_module_file(Path) of
        false ->
            false;
        ModPath ->
            {ok, {_, [{_, CB}]}} = beam_lib:chunks(ModPath, ["CInf"]),
            CompileOpts =  binary_to_term(CB),
            CompileTime = proplists:get_value(time, CompileOpts),
            Src = proplists:get_value(source, CompileOpts),
            not (CompileTime == PrevCompileTime) and (Src == PrevSrc)
    end.

find_module_file(Path) ->
    case file:read_file_info(Path) of
        {ok, _} ->
            Path;
        _ ->
            %% may be the path was changed
            case code:where_is_file(filename:basename(Path)) of
                non_existing ->
                    false;
                NewPath ->
                    NewPath
            end
    end.

tc_loop(_F, 0, List) ->
    List;
tc_loop(F, N, List) ->
    case timer:tc(F) of
        {_T, {'EXIT', Reason}} -> exit(Reason);
        {T, _Result} -> tc_loop(F, N - 1, [T|List])
    end.

tc_loop(_M, _F, _A, 0, List) ->
    List;
tc_loop(M, F, A, N, List) ->
    case timer:tc(M, F, A) of
        {_T, {'EXIT', Reason}} -> exit(Reason);
        {T, _Result} -> tc_loop(M, F, A, N - 1, [T|List])
    end.

get_abstract_code(Module) when is_atom(Module) ->
    case code:ensure_loaded(Module) of
        {module, _} ->
            Beam = code:which(Module),
            get_abstract_code(Beam);
        Error -> Error
    end;
get_abstract_code(Beam) when is_list(Beam) ->
    Basename = filename:basename(Beam, ".beam"),
    case beam_lib:chunks(Beam, [abstract_code]) of
        {ok, {Module, [{abstract_code, {_, AC}}]}} ->
            {ok, Module, Basename, AC};
        Other ->
            Other
    end.

%%%%%%%%%%%%%%%%%%%%%%%%
%%% TRACE FORMATTING %%%
%%%%%%%%%%%%%%%%%%%%%%%%
%% Thanks Geoff Cant for the foundations for this.

trace_format(TraceMsg) ->
    {Type, Pid, {Hour,Min,Sec}, TraceInfo} = extract_info(TraceMsg),
    {FormatStr, FormatArgs} = case {Type, TraceInfo} of
        %% {trace, Pid, 'receive', Msg}
        {'receive', [Msg]} ->
            {"< ~p", [Msg]};
        %% {trace, Pid, send, Msg, To}
        {send, [Msg, To]} ->
            {" > ~p: ~p", [To, Msg]};
        %% {trace, Pid, send_to_non_existing_process, Msg, To}
        {send_to_non_existing_process, [Msg, To]} ->
            {" > (non_existent) ~p: ~p", [To, Msg]};
        %% {trace, Pid, call, {M, F, Args}}
        {call, [{M,F,Args}]} ->
            {"~p:~p~s", [M,F,format_args(Args)]};
        {call, [{M,F,Args}, _]} ->
            {"~p:~p~s", [M,F,format_args(Args)]};
        %% {trace, Pid, return_to, {M, F, Arity}}
        {return_to, [{M,F,Arity}]} ->
            {" '--> ~p:~p/~p", [M,F,Arity]};
        %% {trace, Pid, return_from, {M, F, Arity}, ReturnValue}
        {return_from, [{M,F,Arity}, Return]} ->
            {"~p:~p/~p --> ~p", [M,F,Arity, Return]};
        %% {trace, Pid, exception_from, {M, F, Arity}, {Class, Value}}
        {exception_from, [{M,F,Arity}, {Class,Val}]} ->
            {"~p:~p/~p ~p ~p", [M,F,Arity, Class, Val]};
        %% {trace, Pid, spawn, Spawned, {M, F, Args}}
        {spawn, [Spawned, {M,F,Args}]}  ->
            {"spawned ~p as ~p:~p~s", [Spawned, M, F, format_args(Args)]};
        %% {trace, Pid, exit, Reason}
        {exit, [Reason]} ->
            {"EXIT ~p", [Reason]};
        %% {trace, Pid, link, Pid2}
        {link, [Linked]} ->
            {"link(~p)", [Linked]};
        %% {trace, Pid, unlink, Pid2}
        {unlink, [Linked]} ->
            {"unlink(~p)", [Linked]};
        %% {trace, Pid, getting_linked, Pid2}
        {getting_linked, [Linker]} ->
            {"getting linked by ~p", [Linker]};
        %% {trace, Pid, getting_unlinked, Pid2}
        {getting_unlinked, [Unlinker]} ->
            {"getting unlinked by ~p", [Unlinker]};
        %% {trace, Pid, register, RegName}
        {register, [Name]} ->
            {"registered as ~p", [Name]};
        %% {trace, Pid, unregister, RegName}
        {unregister, [Name]} ->
            {"no longer registered as ~p", [Name]};
        %% {trace, Pid, in, {M, F, Arity} | 0}
        {in, [{M,F,Arity}]} ->
            {"scheduled in for ~p:~p/~p", [M,F,Arity]};
        {in, [0]} ->
            {"scheduled in", []};
        %% {trace, Pid, out, {M, F, Arity} | 0}
        {out, [{M,F,Arity}]} ->
            {"scheduled out from ~p:~p/~p", [M, F, Arity]};
        {out, [0]} ->
            {"scheduled out", []};
        %% {trace, Pid, gc_start, Info}
        {gc_start, [Info]} ->
            HeapSize = proplists:get_value(heap_size, Info),
            OldHeapSize = proplists:get_value(old_heap_size, Info),
            MbufSize = proplists:get_value(mbuf_size, Info),
            {"gc beginning -- heap ~p bytes",
             [HeapSize + OldHeapSize + MbufSize]};
        %% {trace, Pid, gc_end, Info}
        {gc_end, [Info]} ->
            HeapSize = proplists:get_value(heap_size, Info),
            OldHeapSize = proplists:get_value(old_heap_size, Info),
            MbufSize = proplists:get_value(mbuf_size, Info),
            {"gc finished -- heap ~p bytes",
             [HeapSize + OldHeapSize + MbufSize]};
        _ ->
            {"unknown trace type ~p -- ~p", [Type, TraceInfo]}
    end,
    io_lib:format("~n~p:~p:~9.6.0f ~p " ++ FormatStr ++ "~n",
                  [Hour, Min, Sec, Pid] ++ FormatArgs).

extract_info(TraceMsg) ->
    case tuple_to_list(TraceMsg) of
        [trace_ts, Pid, Type | Info] ->
            {TraceInfo, [Timestamp]} = lists:split(length(Info)-1, Info),
            {Type, Pid, to_hms(Timestamp), TraceInfo};
        [trace, Pid, Type | TraceInfo] ->
            {Type, Pid, to_hms(os:timestamp()), TraceInfo}
    end.

to_hms(Stamp = {_, _, Micro}) ->
    {_,{H, M, Secs}} = calendar:now_to_local_time(Stamp),
    Seconds = Secs rem 60 + (Micro / 1000000),
    {H,M,Seconds};
to_hms(_) ->
    {0,0,0}.

format_args(Arity) when is_integer(Arity) ->
    "/"++integer_to_list(Arity);
format_args(Args) when is_list(Args) ->
    "("++string:join([io_lib:format("~p", [Arg]) || Arg <- Args], ", ")++")".

hexstr2bin(S) when is_list(S) ->
    list_to_binary(hexstr2list(S)).

bin2hexstr(Bin) when is_binary(Bin) ->
        << <<(hex(H)),(hex(L))>> || <<H:4,L:4>> <= Bin >>.

hex(C) when C < 10 -> $0 + C;
hex(C) -> $a + C - 10.

hexstr2list([X,Y|T]) ->
    [mkint(X)*16 + mkint(Y) | hexstr2list(T)];
hexstr2list([]) ->
    [].

mkint(C) when $0 =< C, C =< $9 ->
    C - $0;
mkint(C) when $A =< C, C =< $F ->
    C - $A + 10;
mkint(C) when $a =< C, C =< $f ->
    C - $a + 10.
