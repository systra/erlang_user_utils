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

    tc(N, F)            % evaluate F N times and return same result as function tc(N, M, F, A)
    tc(N, M, F, A)      % evaluate M:F(A) N times and return [{total, <total run time>},
                                                              {arithmetic_mean, <mean>},
                                                              {last_result, <result>}]

    tca(N, F)           % evaluate F N times and return same result as function tca(N, M, F, A)
    tca(N, M, F, A)     % evaluate M:F(A) N times and return stats as below:
    
                        (if you have bear library in the ERL_LIB path)
                        
                        [{min,10137},
                         {max,12630},
                         {arithmetic_mean,11537.6},
                         {geometric_mean,11516.435993993891},
                         {harmonic_mean,11494.89018688898},
                         {median,11374},
                         {variance,532961.6000000001},
                         {standard_deviation,730.0421905616141},
                         {skewness,-0.30488239045264337},
                         {kurtosis,-0.9594953873072507},
                         {percentile,[{50,11374},
                                      {75,12114},
                                      {90,12230},
                                      {95,12630},
                                      {99,12630},
                                      {999,12630}]},
                         {histogram,[{11337,4},{12537,5},{14137,1}]},
                         {n,10}]
                         
                     (or simplier form if bear library is not available)
                        
                        [{min,10137},
                         {max,12630},
                         {arithmetic_mean,11537.6},
                         {median,11374},
                         {n,10}]


#### Other helper functions

    p(Term)             % print term using io:format("~p~n", [Term])
    l()                 % load all changed modules on current node
    nl()                % load all changed modules on all known nodes
    mm()                % list modified modules
    mk()                % compile modules specified by Emakefile (if exists) or in current directory
    dmfa()              % run M:F(A1,...,An) on all visible nodes
    decompile(M)        % decompile module or beam file

### Building and installation

Run make and put the line below into ~/.erlang file to enable utilities:

    code:load_abs("/path/to/erlang_user_utils/user_default").

See also http://erlang.org/doc/man/shell_default.html
