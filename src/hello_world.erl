-module(hello_world).
-export([hello_world/0]).

hello_world() ->
	io:fwrite("Hello world!\n").
