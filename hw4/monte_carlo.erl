-module(pi).
-export([monte_carlo/2, calculate/2]).

create_worker(0, _, _) ->
    ok;
create_worker(Actors, WorkPerActor, Parent) ->
    spawn(pi, calculate, [WorkPerActor, Parent]),
    create_worker(Actors - 1, WorkPerActor, Parent).

listen(0, Circle, Total) ->
    8.0 * Circle / Total;
listen(Count, Circle, Total) ->
    receive
        inside ->
            listen(Count - 1, Circle + 1, Total + 1);
        outside ->
            listen(Count - 1, Circle, Total + 1)
    end.

calculate(0, _) ->
    ok;
calculate(Work, Parent) ->
    X = rand:normal(),
    Y = rand:normal(),
    D = X*X + Y*Y,

    if
        D =< 1 ->
            Parent ! inside;
        true ->
            Parent ! outside
    end,
    calculate(Work - 1, Parent).

monte_carlo(Iterations, Actors) ->
    WorkPerActor = round(Iterations / Actors),
    create_worker(Actors, WorkPerActor, self()),
    listen(Iterations, 0, 0).
