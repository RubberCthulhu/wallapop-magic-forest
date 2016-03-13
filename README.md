Solution for Wallapop magic forest programming challenge
========================================================
A magic forest is a set of nodes and edges. An edge connects two distinct nodes. Two nodes cannot be connected by more than one edge. A subset of nodes is a tree if it has the following two properties:
* For any two nodes in the subset there is exactly one series of edges Xi connecting them.
* There is no edge connecting a node from the subset to a node outside the subset.

Write an Erlang module that can calculate the number of trees in a magic forest.

For example, if we have a magic forest with 10 nodes (0-9) and edges 1-2, 3-4, 3-5, 4-5, 6-7, 6-8, and 6-9, countTrees should return 3 as there are three trees (0), (1, 2), and (6, 7, 8, 9) in that magic forest. A subset of nodes (3, 4, 5) is not a tree since there are two series of edges connecting each two nodes. Nodes 3 and 5 are connected via direct edge 3-5 and via series of edges 3-4 and 4-5.

Buuild
------
```
rebar compile
```

Tests
-----
```
rebar eunit
```

Build docs
----------
```
rebar doc
```


