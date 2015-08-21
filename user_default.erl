-module(user_default).

% Usage:
% Compile and add the following to ~/.erlang to enable:
%
% code:load_abs("/Path/To/erlang_user_funs/user_default").

-export([help/0,
         dbg/1, dbg/2, dbg/3, dbg/4, dbgo/0, dbgt/1,
         p/1, lib_path/0, dmfa/3,
         l/0, nl/0, mm/0,
         tc/2, tc/4]).

-import(io, [format/1, format/2]).

-compile(inline).

help() ->
    shell_default:help(),
    format("** user extended commands **~n"),
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
    format("lib_path()      -- print lib path\n"),
    format("p(Term)         -- print term using io:format(\"\~s\", [Term])\n",["~p~n"]),
    format("l()             -- load all changed modules\n"),
    format("nl()            -- load all changed modules on all known nodes\n"),
    format("mm()            -- list modified modules\n"),
    format("dmfa()          -- run M:F(A1,...,An) on all visible nodes\n"),
    format("tc(N, M, F, A)  -- evaluate M:F(A) N times and return {TotalMicSecs, MicSecs/call, Result}\n"),
    format("tc(N, F)        -- evaluate F N times and return {MicSecs, MicSecs/call, Result}\n"),
    ok.

dbgo() ->
    format("Debugging options:\n"),
    format("c  -- clear\n"),
    format("r  -- show return trace (incl. exceptions)\n"),
    format("l  -- trace on local functions\n"),
    format("lr -- trace on local functions with return trace (incl. exceptions)\n"),
    ok.

dbgt(File) ->
    Fun = fun({trace, _, call, {M, F, A}}, _) -> io:format("call: ~w:~w~w~n", [M,F,A]);
             ({trace, _, return_from, {M, F, A}, R}, _) -> io:format("retn: ~w:~w/~w -> ~w~n", [M,F,A,R]);
             (A,B) -> io:format("~w: ~w~n", [A,B]) end,
    dbg:trace_client(file, File, {Fun, []}).


%--- Code Reload

l() ->
    lists:foreach(
      fun(M) ->
              io:format("Loading ~p -> ~p~n", [M, c:l(M)])
      end,
      mm()
     ).

nl() ->
    lists:foreach(
      fun(M) ->
              io:format("Network loading ~p -> ~p~n", [M, c:nl(M)])
      end,
      mm()
     ).

mm() ->
    modified_modules().

%--- Benchmarking -------------------------------------------------------------

tc(N, F) when N > 0 ->
    time_it(fun() -> exit(call(N, N, F, erlang:now())) end).

tc(N, M, F, A) when N > 0 ->
    time_it(fun() -> exit(call(N, N, M, F, A, erlang:now())) end).

%--- Debugging

% Modified version of: https://github.com/eproxus/erlang_user_utilities/blob/master/user_default.erl

dbg(c) -> dbg:stop_clear();
dbg(M) -> dbge({M, '_', '_'}, []).

dbg(c, MSec) when is_number(MSec) ->
    spawn(fun() -> timer:sleep(MSec), dbg:stop_clear(), io:format("debugging has been disabled~n") end),
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

% dbgf(Module, File) when is_list(File) ->
%     {ok,_} = dbg:tracer(port, dbg:trace_port(file, File)),
%     dbg:p(all, [call, running, garbage_collection, timestamp, return_to]),
%     dbg:tpl(Module, [{'_', [], [{return_trace}, {exception_trace}]}]),
%     ok.

%--- Other

p(Term) ->
    io:format("~p~n", [Term]).

dmfa(M, F, As) ->
    Nodes = nodes(),
    case Nodes of
    [] ->
        apply(M, F, As);
    _ ->
        rpc:multicall(M, F, As)
    end.

%--- Private Functions

dbgc(MFA) ->
    dbg:ctp(MFA).

dbge(MFA, O) ->
    {ok, _} = dbg:tracer(),
    dbg:p(all, call),
    dbg:tp(MFA, O).

dbgl(MFA, O) ->
    {ok, _} = dbg:tracer(),
    dbg:p(all, call),
    dbg:tpl(MFA, O).

dbg_rt() -> cx.

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
    Int   = timer:now_diff(Time2, Time1),
    {Int, Int / N, Res}.

lib_path() ->
    KernAppPath = code:where_is_file("kernel.app"),
    string:substr(KernAppPath, 1, string:str(KernAppPath,"kernel") - 1).

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
