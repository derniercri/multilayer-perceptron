-module(utils).
-compile(export_all).

%% convertie un gb_tree en liste triée
gb_trees_to_sorted_list(Gb_Trees) ->
    List = gb_trees:to_list(Gb_Trees),
    F = fun({R1, _}, {R2, _}) ->  R1 =< R2 end,
    Sorted_list = lists:sort(F, List),
    F2 = fun({_, V}) -> V end,
    lists:map(F2, Sorted_list).

seuil(S) ->
    fun (X) when X > S -> 1;
	(_) -> -1
    end.
