-module(fizzbuzz).

-export([calc/2, start/0, stop/1]).

calc(Pid, Value) ->
	Pid ! {analyze, self(), Value},
	receive
		Result -> 
			Result
	end. 

start() -> 
	spawn(fun() -> loop() end).
	
stop(Pid) ->
	Pid ! stop.


analyze(N) ->
	if
		  rem 15 == 0 ->
			fizzbuzz;
		N rem 3 == 0 ->
			fizz;
		N rem 5 == 0 ->
			buzz;
		true ->
			N
	end.

loop() ->
	receive
		{analyze, Caller, Value} ->
			Caller ! analyze(Value),
			loop();
		stop ->
			io:format("i go to my death...~n"),
			ok
	end.