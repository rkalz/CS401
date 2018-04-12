-module(integrate).
-export([integrate/4, compute/5]).

integrate(Coefs, Lower, Upper, Tolerance) ->
    spawn(integrate, compute, [self(), Coefs, Lower, Upper, Tolerance]),
    receive_data(0).

receive_data(Result) ->
    receive {data, Data} -> receive_data(Result + Data)
    after 1000 -> done(Result)
    end.

done(Result) -> Result.

compute(Parent, Coefs, A, B, Tol) ->
    FA = value(Coefs, 0, A, 0),
    FB = value(Coefs, 0, B, 0),

    I1 = ((B-A)/2)*(FA+FB),

    M = (A+B)/2,
    FM = value(Coefs, 0, M, 0),

    I2 = ((B-A)/4)*(FA+2*FM+FB),

    case abs(I1-I2) < 3*(B-A)*Tol of
        true -> Parent ! {data, I2};
        false ->
            spawn(integrate, compute, [Parent, Coefs, A, M, Tol]),
            compute(Parent, Coefs, M, B, Tol)
    end.


value([], _, _, Output) -> Output;
value(Coefs, Exp, Input, Output) ->
    Coef = lists:last(Coefs),
    Add = Coef * math:pow(Input, Exp),
    value(lists:droplast(Coefs), Exp+1, Input, Output+Add).