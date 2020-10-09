-module(key).
-export([generate/0, between/3]).

%The function generate/0 will simply return a random number
%from 1 to 1.000.000.000 (30-bits),
generate() ->
    {A1,A2,A3} = now(),
    random:seed(A1, A2, A3),
    random:uniform(1000000000).

%function will check if a Key is between From and To or
%equal to To
between(Key, From, To) ->
    if
	(From < To) and (Key > From) and (Key =< To) ->
	    true;
	(From > To) and ((Key > From) or (Key =< To)) ->
	    true;
	From == To ->
	    true;
	true ->
	    false
    end.