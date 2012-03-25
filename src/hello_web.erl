-module (hello_web).

-export([start/1, stop/0]).

-define(CONTENT, <<"<html><head><title>Herro wuruhld</title></head>huuuuuuuuuhuh<body></body></html>">>).

start(Port) -> 
	mochiweb_http:start([
		{port, Port}, 
		{loop, fun(Req) ->
			Req:ok({"text/html", ?CONTENT}) end
		}
	]).

stop() -> 
	mochiweb_http:stop().