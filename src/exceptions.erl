-module(exceptions).

-compile(export_all).

throws(F) ->
	try F() of 
		_ -> ok
	catch 
		exception:Reason -> {throw, caught, Reason};
		error:Error -> {error, caught, Error}
	end.

sword(1) -> throw(slice);
sword(2) -> error(cut_arm);
sword(3) -> exit(cut_leg);
sword(4) -> throw(punch);
sword(5) -> exit(cross_bridge).

black_knight(Attack) when is_function(Attack, 0) ->
	try Attack() of
		_ -> "None shall pass."
	catch
		throw:slice -> "It is but a scratch.";
		error:cut_arm -> "I've had worse.";
		exit:cut_leg -> "Come on you pansy!";
		_:_ -> "Just a flesh wound."
	after
		io:format("*squirts blood from bloody stump*\n")
	end.