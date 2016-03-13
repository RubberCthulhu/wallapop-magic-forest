%%%-------------------------------------------------------------------
%%% @author Danil Onishchenko <danil.onishchenko.info@gmail.com>
%%% @copyright (C) 2016, Danil Onishchenko
%%% @doc
%%%
%%% @end
%%% Created : 12 Mar 2016 by Danil Onishchenko <danil.onishchenko.info@gmail.com>
%%%-------------------------------------------------------------------
-module(magic_forest).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([trees/2, circles/2, subsets/2]).

%%%===================================================================
%%% API
%%%===================================================================

-spec trees(N, Edges) -> {ok, Trees} | {error, Reason} when
      N :: non_neg_integer(),
      Edges :: [Edge],
      Edge :: {non_neg_integer(), non_neg_integer()},
      Trees :: [Tree],
      Tree :: {tree, Start, Edges},
      Start :: non_neg_integer(),
      Reason :: badarg.

trees(N, Edges) ->
    filter_subsets_helper(tree, N, Edges).

-spec circles(N, Edges) -> {ok, Circles} | {error, Reason} when
      N :: non_neg_integer(),
      Edges :: [Edge],
      Edge :: {non_neg_integer(), non_neg_integer()},
      Circles :: [Circle],
      Circle :: {circle, Start, Edges},
      Start :: non_neg_integer(),
      Reason :: badarg.

circles(N, Edges) ->
    filter_subsets_helper(circle, N, Edges).

filter_subsets_helper(Type, N, Edges) ->
    case subsets(N, Edges) of
	{ok, Subsets} ->
	    Subsets1 = lists:filter(
			 fun ({T, _, _}) ->
				 T == Type
			 end,
			 Subsets),
	    {ok, Subsets1};
	Error ->
	    Error
    end.

-ifdef(TEST).
subsets_test() ->
    ?assertEqual({error, badarg}, subsets_helper(0, [{1, 2}])),

    ?assertEqual({ok, sort_subsets([])}, subsets_helper(0, [])),
    ?assertEqual({ok, sort_subsets([{tree, 0, []}])}, subsets_helper(1, [])),
    ?assertEqual({ok, sort_subsets([{tree, 0, []}, {tree, 1, []}])}, subsets_helper(2, [])),

    ?assertEqual({ok, sort_subsets([{tree, 0, [{0, 1}]}])}, subsets_helper(2, [{0, 1}])),
    ?assertEqual({ok, sort_subsets([{tree, 0, []}, {tree, 1, [{1, 2}]}])}, subsets_helper(3, [{1, 2}])),
    
    ?assertEqual({ok, sort_subsets([{circle, 0, [{0, 1}, {0, 2}, {1, 2}]}])}, subsets_helper(3, [{0, 1}, {0, 2}, {1, 2}])),
    
    ?assertEqual({ok, sort_subsets([{tree, 0, []},
				    {tree, 1, [{1, 2}]},
				    {circle, 3, [{3, 4}, {3, 5}, {4, 5}]},
				    {tree, 6, [{6, 7}, {6, 8}, {6, 9}]}])},
		 subsets_helper(10, [{1, 2}, {3, 4}, {3, 5}, {4, 5}, {6, 7}, {6, 8}, {6, 9}])).

subsets_helper(N, Edges) ->
    subsets_helper(fun subsets/2, N, Edges).

subsets_helper(Gen, N, Edges) ->
    case Gen(N, Edges) of
	{ok, Subsets} ->
	    {ok, sort_subsets(Subsets)};
	Else ->
	    Else
    end.

sort_subsets(Subsets) ->
    lists:sort([{Type, N, lists:sort([fixpos(X, Y) || {X, Y} <- List])} || {Type, N, List} <- Subsets]).

fixpos(X, Y) when X =< Y ->
    {X, Y};
fixpos(X, Y) ->
    {Y, X}.

trees_test() ->
    ?assertEqual({ok, sort_subsets([{tree, 0, []},
				    {tree, 1, [{1, 2}]},
				    {tree, 6, [{6, 7}, {6, 8}, {6, 9}]}])},
		 subsets_helper(fun trees/2, 10, [{1, 2}, {3, 4}, {3, 5}, {4, 5}, {6, 7}, {6, 8}, {6, 9}])).

circles_test() ->    
    ?assertEqual({ok, sort_subsets([{circle, 3, [{3, 4}, {3, 5}, {4, 5}]}])},
		 subsets_helper(fun circles/2, 10, [{1, 2}, {3, 4}, {3, 5}, {4, 5}, {6, 7}, {6, 8}, {6, 9}])).
-endif.

-spec subsets(N, Edges) -> {ok, Subsets} | {error, Reason} when
      N :: non_neg_integer(),
      Edges :: [Edge],
      Edge :: {non_neg_integer(), non_neg_integer()},
      Subsets :: [Subset],
      Subset :: {Type, Start, Edges},
      Type :: tree | circle,
      Start :: non_neg_integer(),
      Reason :: badarg.

subsets(N, Edges) when N >= 0 ->
    case make_matrix(N, Edges) of
	{ok, Matrix} ->
	    Subsets = subsets1(Matrix),
	    {ok, Subsets};
	Error ->
	    Error
    end.

subsets1(Matrix) ->
    subsets2(Matrix, 0, []).

subsets2(Matrix, I, Acc) ->
    case I < array:size(Matrix) of
	true ->
	    case subset(I, Matrix) of
		{false, Matrix1} ->
		    subsets2(Matrix1, I + 1, Acc);
		{Subset, Matrix1} ->
		    subsets2(Matrix1, I + 1, [Subset | Acc])
	    end;
	false ->
	    Acc
    end.

subset(N, Matrix) ->
    case matrix_get_used(N, Matrix) of
	true ->
	    {false, Matrix};
	false ->
	    subset(N, 0, Matrix, [], {tree, N, []})
    end.

subset(I, J, Matrix, Nodes, Subset) ->
    case J < array:size(Matrix) of
	true ->
	    case matrix_get_used(I, J, Matrix) of
		true ->
		    subset(I, J + 1, Matrix, Nodes, Subset);
		false ->
		    Matrix1 = matrix_set_used(I, J, Matrix),
		    case matrix_has_edge(I, J, Matrix1) of
			true ->
			    case is_prev_node(J, Nodes) of
				true ->
				    subset(I, J + 1, Matrix1, Nodes, Subset);
				false ->
				    case is_circle(J, Nodes) of
					true ->
					    {_, N, Edges} = Subset,
					    Subset1 = {circle, N, [{I, J} | Edges]},
					    subset(I, J + 1, Matrix1, Nodes, Subset1);
					false ->
					    {Type, N, Edges} = Subset,
					    Subset1 = {Type, N, [{I, J} | Edges]},
					    {Subset2, Matrix2} = subset(J, 0, Matrix1, [I | Nodes], Subset1),
					    subset(I, J + 1, Matrix2, Nodes, Subset2)
				    end
			    end;
			false ->
			    subset(I, J + 1, Matrix1, Nodes, Subset)
		    end
	    end;
	false ->
	    {Subset, Matrix}
    end.

is_prev_node(N, [X | _]) when N == X ->
     true;
is_prev_node(_, _) ->
    false.

is_circle(N, [_ | Nodes]) ->
    lists:member(N, Nodes);
is_circle(_, _) ->
    false.

make_matrix(N, Edges) ->
    Test = fun (X) -> (X >= 0) and (X < N) end,
    case lists:all(Test, [X || {X, _} <- Edges] ++ [X || {_, X} <- Edges]) of
	true ->
	    Row = array:new(N, [{default, {false, 0}}]),
	    Matrix = array:new(N, [{default, Row}]),
	    make_matrix1(Matrix, Edges);
	false ->
	    {error, badarg}
    end.

make_matrix1(Matrix, [{I, J} | Edges]) ->
    Matrix1 = matrix_set(Matrix, I, J, 1),
    Matrix2 = matrix_set(Matrix1, J, I, 1),
    make_matrix1(Matrix2, Edges);
make_matrix1(Matrix, []) ->
    {ok, Matrix}.

matrix_set(Matrix, I, J, Value) ->
    Row = array:get(I, Matrix),
    {Used, _} = array:get(J, Row),
    Row1 = array:set(J, {Used, Value}, Row),
    array:set(I, Row1, Matrix).

matrix_get(Matrix, I, J) ->
    Row = array:get(I, Matrix),
    {_, Edge} = array:get(J, Row),
    Edge == 1.

matrix_set_used(I, J, Matrix) ->
    case (I < array:size(Matrix)) or (J < array:size(Matrix)) of
	true ->
	    Row = array:get(I, Matrix),
	    {_, Edge} = array:get(J, Row),
	    Row1 = array:set(J, {true, Edge}, Row),
	    Matrix1 = array:set(I, Row1, Matrix),

	    Row2 = array:get(J, Matrix1),
	    {_, Edge2} = array:get(I, Row2),
	    Row21 = array:set(I, {true, Edge2}, Row2),
	    array:set(J, Row21, Matrix1);
	false ->
	    Matrix
    end.

matrix_get_used(I, Matrix) ->
    lists:any(
      fun (J) ->
	      matrix_get(Matrix, I, J) and matrix_get_used(I, J, Matrix)
      end,
      lists:seq(0, array:size(Matrix) - 1)).

matrix_get_used(I, J, Matrix) ->
    case (I < array:size(Matrix)) or (J < array:size(Matrix)) of
	true ->
	    Row = array:get(I, Matrix),
	    {Used, _} = array:get(J, Row),
	    Used;
	false ->
	    false
    end.

matrix_has_edge(I, J, Matrix) ->
    matrix_get(Matrix, I, J).


