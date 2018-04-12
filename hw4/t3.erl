-module(t3).
-export([newgame/0, playwith/1, tell/1, tell_server/1, notification_server/0, server_select_first/4, placetoken/1]).

newgame() ->
    receive
        free -> receive in_game -> io:format("Waiting for a player...~n", []), standby_server() end;
        in_game ->
            io:format("You're already in a game!~n", []),
            self() ! in_game,
            io:format("",[])
    after 10 ->
        register(t3, self()),
        io:format("Waiting for a player...~n", []),
        standby_server()
    end.

standby_server() ->
    receive {Player, Pid, ready} ->
        io:format("~s wants to play! ~n", [Player]),
        Pid ! {ready, self()},
        LocalNotServer = spawn(t3, notification_server, []),
        self() ! {not_server, LocalNotServer},
        receive {rem_not_server, ServerPid} ->
            RemoteNotServer = ServerPid
        end,
        spawn(t3, tell_server, [Pid]),
        spawn(t3, server_select_first, [LocalNotServer, RemoteNotServer, self(), Pid]),
        self() ! in_game,
        io:format("",[])
    end.

server_select_first(LocNotSrv, RemNotSrv, LocPlayer, RemPlayer) ->
    LocPlayer ! {server, self()},
    RemPlayer ! {server, self()},
    case rand:uniform(2) of
        1 ->
            LocNotSrv ! {notification, "You go first!"},
            RemNotSrv ! {notification, "They go first!"},
            game_loop(LocPlayer, LocPlayer, LocNotSrv, RemPlayer, RemNotSrv, ordsets:new(), ordsets:new());
        2 ->
            LocNotSrv ! {notification, "They go first!"},
            RemNotSrv ! {notification, "You go first!"},
            game_loop(RemPlayer, LocPlayer, LocNotSrv, RemPlayer, RemNotSrv, ordsets:new(), ordsets:new())
    end.

game_loop(Current, Local, LocNot, Remote, RemNot, LocalMoves, RemoteMoves) ->
    Whitelist = ordsets:from_list(['a1','a2','a3','b1','b2','b3','c1','c2','c3']),
    receive
        {Player, move, Coordinate} ->
            if
                Player == Current ->
                    Other = case Current of
                        Local -> Remote;
                        Remote -> Local
                    end,
                    CurNot = case Current of
                        Local -> LocNot;
                        Remote -> RemNot
                    end,
                    OthNot = case Other of
                        Local -> LocNot;
                        Remote -> RemNot
                    end,
                    CurMovs = case Current of
                        Local -> LocalMoves;
                        Remote -> RemoteMoves
                    end,
                    OthMovs = case CurMovs of
                        LocalMoves -> RemoteMoves;
                        RemoteMoves -> LocalMoves
                    end,

                    case ordsets:is_element(Coordinate, Whitelist) of
                        false ->
                            CurNot ! {notification, "Invalid move! Try again."},
                            game_loop(Current, Local, LocNot, Remote, RemNot, LocalMoves, RemoteMoves);
                        true -> ok
                    end,
                    case ordsets:is_element(Coordinate, CurMovs) of
                        true ->
                            CurNot ! {notification, "You've already made that move! Try again."},
                            game_loop(Current, Local, LocNot, Remote, RemNot, LocalMoves, RemoteMoves);
                        false -> ok
                    end,
                    case ordsets:is_element(Coordinate, OthMovs) of
                        true ->
                            CurNot ! {notification, "The other player has already made that move! Try again."},
                            game_loop(Current, Local, LocNot, Remote, RemNot, LocalMoves, RemoteMoves);
                        false -> ok
                    end,

                    NewCurMoves = ordsets:add_element(Coordinate, CurMovs),
                    case check_for_victory(NewCurMoves) of
                        true -> end_game(Local, Remote, CurNot, OthNot, "Win");
                        false -> ok
                    end,
                    case check_for_draw(NewCurMoves, OthMovs, Whitelist) of
                        true -> end_game(Local, Remote, CurNot, OthNot, "Draw");
                        false -> ok
                    end,

                    OthNot ! {notification, "It's your turn!"},
                    CurNot ! {notification, "It's their turn!"},
                    case Current of
                        Local ->
                            game_loop(Other, Local, LocNot, Remote, RemNot, NewCurMoves, RemoteMoves);
                        Remote ->
                            game_loop(Other, Local, LocNot, Remote, RemNot, LocalMoves, NewCurMoves)
                    end;
                true ->
                    NotSrv = case Player of
                        Local -> LocNot;
                        Remote -> RemNot
                    end,

                    NotSrv ! {notification, "It's not your turn!"},
                    game_loop(Current, Local, LocNot, Remote, RemNot, LocalMoves, RemoteMoves)
            end
    end.

end_game(Local, Remote, CurNot, OthNot, Case) ->
    case Case of
        "Win" ->
            CurNot ! {notification, "You win!"},
            OthNot ! {notification, "You lose!"};
        "Draw" ->
            CurNot ! {notification, "It's a draw!"},
            OthNot ! {notification, "It's a draw!"}
    end,

    CurNot ! free,
    OthNot ! free,
    Local ! free,
    Remote ! free, ok.

check_for_victory(Moves) ->
    ordsets:is_subset(ordsets:from_list(['a1','b1','c1']), Moves) or
    ordsets:is_subset(ordsets:from_list(['a2','b2','c2']), Moves) or
    ordsets:is_subset(ordsets:from_list(['a3','b3','c3']), Moves) or
    ordsets:is_subset(ordsets:from_list(['a1','a2','a3']), Moves) or
    ordsets:is_subset(ordsets:from_list(['b1','b2','c3']), Moves) or
    ordsets:is_subset(ordsets:from_list(['c1','c2','c3']), Moves) or
    ordsets:is_subset(ordsets:from_list(['a1','b2','c3']), Moves) or
    ordsets:is_subset(ordsets:from_list(['a3','b2','c1']), Moves).

check_for_draw(MovesA, MovesB, Whitelist) ->
    Moves = ordsets:union(MovesA, MovesB),
    ordsets:is_subset(Whitelist, Moves).

notification_server() ->
    receive
        {notification, Data} ->
            io:format("Game: ~s~n", [Data]),
            notification_server();
        free -> ok
    end.

playwith(Opponent) ->
    receive
        in_game ->
            io:format("You're already in a game!~n", []),
            self() ! in_game,
            io:format("",[]);
        free -> receive in_game -> playwith(Opponent) end
    after 10 ->
        search_for(Opponent)
    end.

search_for(Opponent) ->
    case net_adm:ping(Opponent) of
        pong ->
            io:format("~s is online!~n", [Opponent]),
            {t3, Opponent} ! {node(), self(), ready},
            receive {ready, Pid} ->
                io:format("~s is ready to play!~n", [Opponent]),
                spawn(t3, tell_server, [Pid]),
                NotServer = spawn(t3, notification_server, []),
                self() ! {not_server, NotServer},
                {t3, Opponent} ! {rem_not_server, NotServer},
                self() ! in_game
            after 1000 ->
                io:format("~s is busy!~n", [Opponent])
            end;
        pang ->
            io:format("~s is offline!~n", [Opponent])
    end,

    io:format("",[]).

tell_server(Opponent) ->
    Opponent ! {remote_server, self()},
    receive {message, Player, Contents} ->
        io:format("~s: ~s~n", [Player, Contents]),
        tell_server(Opponent)
    end.

tell(Message) ->
    receive
        free ->
            receive {remote_server, _} -> ok end,
            io:format("Not connected to a game.~n", []);
        {remote_server, Pid} ->
            Pid ! {message, node(), Message},
            io:format("~s: ~s~n", [node(), Message])
    after 1000 ->
        io:format("Not connected to a game.~n", [])
    end.

placetoken(Coordinate) ->
    receive
        {server, Server} ->
            Server ! {self(), move, Coordinate},
            self() ! {server, Server},
            io:format("",[]);
        free ->
            receive {server, _} -> ok end,
            io:format("Not connected to a game.~n", []),
            self() ! free
    after 1000 ->
        io:format("Not connected to a game.~n", [])
    end.
