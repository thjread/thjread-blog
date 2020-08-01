----
title: What is a quasi-category?
modified: 2020-08-01
meta_description: TODO
tags: Category theory, Infinity categories
----

Quasi-categories are one of the earliest and most important models for (weak) $(\infty, 1)$-categories -- that is, an $\infty$-category in which all $n$-morphisms for $n \ge 2$ are equivalences, and where composition and laws such as associativity are only defined up to higher morphisms. <!-- TODO explain this much better --> <!-- TODO credit Joyal, Lurie, Vogtmansdfj etc? -->
The definition of a quasi-category turns out to be less horrendously complicated than you might expect! The key is to understand simplicial sets, which take a combinatorial approach to topology. Then a quasi-category is a simplicial set satisfying a condition that asks for certain "compositions of morphisms" to exist. We'll start with a quick overview of simplicial sets, emphasising geometric intuition, before explaining the precise definition of a quasi-category.

<!--more-->

## Simplicial sets

Simplicial sets are a combinatorial approach to describing a kind of abstract triangulated topological space, created by gluing together simplices (points, lines, triangles, tetrahedra etc.). They are very closely related to the simplicial complexes you might have met in algebraic topology (but differ in that they allow degenerate simplices).<!-- TODO move parenthetical to a footnote, or explain later and flag that here-->

<!-- picture here -->

Let $\Delta$ be the category with objects the finite non-empty ordered sets
\[[n] = \{0, 1, \dotsc, n\}\]
and morphisms given by (weakly) order-preserving functions (i.e. a morphism $f : [m] \to [n]$ is a function $f : \{0, \dotsc, m\} \to \{0, \dotsc, n\}$ such that $f(i) \le f(j)$ for all $i \le j$). Note that $[n]$ can be interpreted as a category with objects $0, \dotsc, n$ and a unique morphism $i \to j$ for every $i \le j$, and from this perspective $\Delta$ is a full subcategory of $\text{Cat}$.

A simplicial set is a functor
\[A : \Delta^\text{op} \to \text{Set} \text{.}\]
Geometrically, we interpret $A([n])$ as the set of $n$-dimensional simplices making up the simplicial set. We usually denote this set $A_n$. We interpret a morphism $f : [m] \to [n]$ (i.e. an order-preserving function $\{0, \dotsc, m\} \to \{0, \dotsc, n\}$) as describing a way of embedding an $m$-simplex into an $n$-simplex. Then the function $A(f) : A_n \to A_m$ describes how to take an $n$-simplex from the set and identify the corresponding $m$-simplex living inside it.

<!-- pictures -->

We write $\text{sSet}$ for the category $\text{Set}^{\Delta^\text{op}}$ of simplicial sets. In particular a morphism between two simplicial sets $A$ and $B$ is given by a natural transformation $\alpha$ between the functors. That is, for each $n$ we have a map $\alpha([n]) : A_n \to B_n$ taking $n$-simplices of $A$ to $n$-simplices of $B$, such that a face of a simplex in $A$ is mapped to the correpsonding face of the image of the simplex in $B$.


We write $\Delta[n]$ for the *standard $n$-simplex*, which is given by the representable functor $\Delta({-}, [n])$.

<!-- mention degenerate simplices? maybe pretty diagram with all the 2-simplices in the standard 2-simplex -->
In order to understand the definition of a quasi-category, we don't need a great understanding of the subtleties of the definition. It'll suffice to think of a simplicial set as a shape made up of simplices glued together, and where each simplex comes with an ordering on its vertices. A morphism of simplicial sets from $A$ to $B$ is like identifying an $A$-shaped subset of $B$, with the caveat that you are allowed to collapse simplices of $A$ to degenerate simplices (for example mapping a triangle onto a line). <!-- expand this? TODO move this to the start -->

<!-- pictures -->

<!-- mention maybe future post?-->

## The nerve of a category

At the moment it's probably unclear how a simplicial set could be anything like a category. We'll start by seeing how to represent an ordinary category as a simplicial set, via the nerve construction. After that we'll explain how to generalise this to quasi-categories.

The idea is that a category already looks a bit like a simplicial set, with a vertex for each object and a $1$-simplex for each morphism. To form the nerve, we also add in an $n$-simplex for each string of $n$ composable morphisms.

Let $\mathcal{C}$ a category. We define the nerve $N\!\mathcal{C}$ to be the restriction of the representable functor $\text{Cat}({-}, \mathcal{C}) : \text{Cat}^\text{op} \to \text{Set}$ to $\Delta$. So the set $N\!\mathcal{C}_n$ of $n$-simplices is the set of functors $[n] \to \mathcal{C}$, which we can identify as the set of sequences of $n$ composable morphisms in $\mathcal{C}$.

<!-- picture -->

It's fairly straightforward to check that the nerve construction gives a fully faithful embedding of $\text{Cat}$ inside $\text{sSet}$ -- there's a bijection between maps $N\!\mathcal{C} \to N\!\mathcal{D}$ and functors $\mathcal{C} \to \mathcal{D}$.

We would like to characterise those simplicial sets which are (isomorphic to) the nerve of some category. The *spine* $I[n]$ of the standard $n$-simplex is the simplicial complex with vertices $\{0, \dotsc, n\}$ and edges $\langle 0, 1 \rangle, \dotsc, \langle n-1, n \rangle$. It lives inside the standard $n$-simplex $\Delta[n]$ in the natural way. Now we find that a simplicial set $A$ is (isomorphic to) the nerve of some category iff every map from a spine $I[n] \to A$ extends uniquely along the natural inclusion to a map from an $n$-simplex $\Delta[n] \to A$.

<!-- diagram -->

Checking this is a nice exercise. The rough idea is that a map $I[n] \to N\mathcal{C}$ corresponds to a sequence of $n$ edges in $N\mathcal{C}$, which corresponds to a sequence of $n$ composable morphisms in $\mathcal{C}$, which corresponds to an $n$-simplex in $N \mathcal{C}$.

## Quasi-categories

The definition of a quasi-category is a generalisation of this idea. To get a category, we ask for every spine to extend uniquely to a simplex, since a sequence of ($1$-)morphisms in a category has a unique composite. In a weak $(\infty, 1)$-category we don't have uniquely defined composites, and we also want to handle composing arrangements of higher dimensional morphisms. 

The $k$th *horn* of the standard $n$-simplex (for some $0 \le k \le n$), denoted $\Lambda^k[n]$, is the largest simplicial set contained in $\Delta[n]$ which does not include the face opposite the vertex $k$. A horn of the $3$-simplex really looks like a (low poly) horn! While a horn of the $2$-simplex looks like two edges joined at a vertex.

<!-- picture -->

We call $\Lambda^k[n]$ an *inner horn* if $0 < k < n$. We can now finally state the definition of a quasi-category:

::: definition
**Definition** (quasi-category)

A quasi-category is a simplicial set $A$ in which every inner horn can be extended (not necessarily uniquely) to a simplex. That is, for every map $\Lambda^k[n] \to A$ ($0 < k < n$), there exists a map $\Delta[n] \to A$ such that the following diagram commutes:

<!-- diagram --> 
:::

- examples of quasicategories? e.g. categories
- examples of composing together 1-cells, 2-cells?
- compare to Kan complex / infty-groupoid?

## References and further reading

References and further reading - Riehl Verity book, some guide to simplicial sets
