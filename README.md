UnionFind
=========
This is union-find a.k.a. disjoint-set implementation in Scala which you can use in functional wasy - without any mutable state.
In fact this library uses mutable variables in some places to make some operations faster, but for outer worls it presents read-only interface.
You can find more theory here: http://en.wikipedia.org/wiki/Disjoint-set_data_structure, but the main idea is that we have N items that initially belong to N groups. Then we need to perform 2 actions on these items as efficeintly as possible:

- join 2 items so that they form one group. If these items have been grouped with another items before then all these items make a single group.  
- find the group of some item 

It also would be nice to find out which groups do we have now but its performance isn't that important - this operation can be performed only a few times during the whole lifetime of UnionFind structure.

This library was written as a utility for some tasks of "Algorithms: Design and Analysis, Part 2" course (https://www.coursera.org/course/algo2)

##Usage

Here are code samples how you can use UnionFind to do this operations:

     val uf = new UnionFind(5)
	 val uf2 = uf.join(0, 3).join(3, 4)
	 assert(uf2.find(0) == uf2.find(3)) 
	 assert(uf2.find(0) == uf2.find(4)) 
	 assert(uf2.find(0) != uf2.find(1)) 
	 //and so on

You can find more examples in unit tests - UnionFindSuite.scala

##Performance

TODO: Write here some analisys

##License

Licensed under Apache 2.0 license:
http://www.apache.org/licenses/LICENSE-2.0.txt

