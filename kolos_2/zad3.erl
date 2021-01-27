-module(zad3).

-export([printer/2, start/2]).

printer(I, N) ->
    receive
        X ->
            io:format("(~w, ~w)", [I, X]),
            if 
                (I rem 2) == 0 -> list_to_atom("printer" ++ integer_to_list((I) rem N)) ! (2 * X);
                (I rem 2) == 1 -> list_to_atom("printer" ++ integer_to_list((I) rem N)) ! (X - 1)
            end,
            printer(I, N)
    end.

start(N, X) ->
    [register(list_to_atom("printer"++integer_to_list(K)), spawn(zad3, printer, [K+1, N])) || K <-lists:seq(0, N-1)],
    printer0 ! X.

