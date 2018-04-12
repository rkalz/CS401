-module(integrate).
-export([integrate/3, compute/4, function/1]).

function(Input) ->
    Input*Input + 2*Input + 1.

integrate(Lower, Upper, Tolerance) ->
    spawn(integrate, compute, [self(), Lower, Upper, Tolerance]),
    receive_data(0).

receive_data(Result) ->
    receive {data, Data} -> receive_data(Result + Data)
    after 1000 -> done(Result)
    end.

done(Result) -> Result.

compute(Parent, A, B, Tol) ->
    M = (A+B)/2,
    FA = function(A),
    FB = function(B),
    FM = function(M),

    I1 = ((B-A)/2)*(FA+FB),
    I2 = ((B-A)/4)*(FA+2*FM+FB),

    case abs(I1-I2) < 3*(B-A)*Tol of
        true -> Parent ! {data, I2};
        false ->
            spawn(integrate, compute, [Parent, A, M, Tol]),
            compute(Parent, M, B, Tol)
    end.