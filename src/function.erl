-module(function).
-compile(export_all).

seuil(S) ->
    fun (X) when X > S -> 1;
	(_) -> -1
    end.
