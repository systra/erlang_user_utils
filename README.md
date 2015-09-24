## A collection of handy user utilities for the Erlang shell.

### Usage

#### Debugger helper functions

    dbg(c)              %   clear all traces = stop tracing
    dbg(c, T)           %   schedule clearing all traces after T msec

    dbg(M)              %   trace on module M
    dbg(M, O)           %   trace on module M with options O
    dbg(M, F)           %   trace on function M:F
    dbg(M, F, O)        %   trace on function M:F with options O
    dbg(M, F, A)        %   trace on function M:F(A)
    dbg(M, F, A, O)     %   trace on function M:F(A) with options O
    dbgo()              %   print debugging options help
    dbgt(File)          %   print trace data read from File

The options for debugging are:

    c                   %   clear
    r                   %   show return trace (including exceptions)
    l                   %   trace on local functions
    lr                  %   trace on local functions with return trace (including exceptions)

#### Benchmarking helper functions

    tc(N, M, F, A)      % evaluate M:F(A) N times and return {TotalMicSecs, MicSecs/call, Result}
    tc(N, F)            % evaluate F N times and return {MicSecs, MicSecs/call, Result}


#### Other helper functions

    p(Term)             % print term using io:format("~p~n", [Term])
    l()                 % load all changed modules on current node
    nl()                % load all changed modules on all known nodes
    mm()                % list modified modules
    mk()                % compile modules specified by Emakefile (if exists) or in current directory
    dmfa()              % run M:F(A1,...,An) on all visible nodes

### Building and installation

Run make and put the line below into ~/.erlang file to enable utilities:

    code:load_abs("/path/to/erlang_user_utils/user_default").

See also http://erlang.org/doc/man/shell_default.html
