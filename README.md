# GraphFT
Code specifying a fortran directed graph derived type.

Graphs are declared with
```
type(Graph) :: my_graph
```
and can be initialised either by invoking
```
call Graph(my_graph, n_nodes)
```
where n_nodes specifies the initial number of nodes, or by simply adding nodes one at a time with repeated calls to
```
my_graph%add_node()
```

By default, nodes are indexed starting from 1. If nodes are removed, the graph can be reindexed with 
```
call my_graph%reindex_nodes()
```
Edges are added with calls to
```
call my_graph%add_edge(start_id, end_id, [weight])
```
where weight is an optional (integer) distance between nodes.

Some rudimentary analysis is provided, with
```
my_graph%connections()
call my_graph%shortes_distance(start_node, end_node,d,[index])
```
The former outputs an array of the number of connections to each node, and the latter calculates the shortest path between two nodes using Djikstra's algorithm.
