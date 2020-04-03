----
title: Limits commute with limits
modified: 2020-04-04
meta_description: How do you prove that limits commute with limits, and what are the precise conditions for this to hold?
tags: Category theory
----

It's a well known fact in category theory that limits commute with limits (and dually colimits commute with colimits). That is, given a functor $F : I \times J \to C$ we have
\[\lim_i \lim_j F(i, j) \cong \lim_j \lim_i F(i, j)\]
under appropriate conditions.

<!--more-->

If $C$ has all $I$-shaped limits and all $J$-shaped limits then there's an elegant proof using uniqueness of adjoints. However the theorem is true under weaker conditions. We have

::: theorem
**Theorem** (Limits commute with limits). Let $F : I \times J \to C$ be a functor (where $I$ and $J$ are small categories). If $\lim_j F(i, j)$ exists for all $j \in J$ then we find that
$\lim_i \lim_j F(i, j)$ exists iff $\lim_{(i, j)} F(i, j)$ exists, in which case they are canonically isomorphic.
:::

In particular
\[\lim_i \lim_j F(i, j) \cong \lim_j \lim_i F(i, j)\]
if both sides exist.

Since this version of the theorem is surprisingly hard to track down, I thought it was worth writing up a statement and proof. Emily Riehl's Category Theory in Context (Theorem 3.8.1) comes close, although it is missing the existence part. The only reference I've found is as a special case of "Fubini for (co)ends" from Mac Lane's Categories for the Working Mathematician, but the proof he gives doesn't explain why the existence result holds.

I should briefly clarify what $\lim_i \lim_j F(i, j)$ actually means. A morphism $f : j \to j'$ induces a natural transformation $F({-}, j) \Rightarrow F({-}, j')$, which induces a morphism $\lim_i F(i, f) : \lim_i F(i, j) \to \lim_i F(i, j')$. So the limits $\lim_i F(i, j)$ in fact assemble into a functor $J \to C$, and $\lim_j \lim_i F(i, j)$ is the limit of this functor. (In more sophisticated language, there is a bijection between functors $I \times J \to C$ and functors $I \to C^J$. Since limits in functor categories are pointwise, if $\lim_i F(i, j)$ exists as a limit in $C$ for every $j \in J$ then $\lim_i F(i, {-})$ exists as a limit in $C^J$. So we can define $\lim_j \lim_i F(i, j)$ to be the limit of this resulting functor.)

## Proof

We use the usual trick of using Yoneda to reduce to $\text{Set}$.

First let us prove the theorem in the case $C = \text{Set}$. Since $\text{Set}$ is complete, all (small) limits exist, and so if you want you're free to use the proof by uniqueness of adjoints I mentioned earlier. Here I give a very concrete proof using the explicit description of limits in $\text{Set}$, which I think is enlightening in a different way.
