-module(port_compiler).
-export([pre_compile/2, clean/2]).

pre_compile(_, _) -> execute("make compile-port").

clean(_, _) -> execute("make clean-port").

execute(Cmd) ->
	case rebar_utils:sh(Cmd, [return_on_error, use_stdout]) of
		{ok, _} -> ok;
		{error, {Status, _Log}} -> {error, {exit_status, Status}}
	end.

