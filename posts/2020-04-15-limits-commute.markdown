----
title: In what sense do limits commute with limits?
modified: 2020-04-15
meta_description: How do you prove that limits commute with limits, and what are the precise conditions for this to hold?
tags: Category theory
----

It is a well known fact in category theory that limits commute with limits (and dually colimits commute with colimits). That is, given a functor $F : I \times J \to C$ we have
\[\lim_i \lim_j F(i, j) \cong \lim_j \lim_i F(i, j)\]
under appropriate conditions.

<!--more-->

If $C$ has all $I$-shaped limits and all $J$-shaped limits then there's an elegant proof using uniqueness of adjoints. However the theorem is true under weaker conditions. We have

::: theorem
**Theorem** (Limits commute with limits)

Let $F : I \times J \to C$ be a functor (where $I$ and $J$ are small categories). If $\lim_j F(i, j)$ exists for all $i \in I$ then we find that
$\lim_i \lim_j F(i, j)$ exists iff $\lim_{(i, j)} F(i, j)$ exists, in which case they are canonically isomorphic.

In particular
\[\lim_i \lim_j F(i, j) \cong \lim_j \lim_i F(i, j)\]
if both sides exist.
:::

Since this version of the theorem is surprisingly hard to track down, I thought it was worth writing up a statement and proof. Emily Riehl's Category Theory in Context (Theorem 3.8.1) comes close, although it is missing the existence part. The only reference I've found is as a special case of "Fubini for (co)ends" from Mac Lane's Categories for the Working Mathematician, but the proof he gives doesn't explain why the existence result holds.

I should briefly clarify what $\lim_i \lim_j F(i, j)$ actually means. A morphism $f : j \to j'$ induces a natural transformation $F({-}, j) \Rightarrow F({-}, j')$, which induces a morphism $\lim_i F(i, f) : \lim_i F(i, j) \to \lim_i F(i, j')$. So the limits $\lim_i F(i, j)$ in fact assemble into a functor $J \to C$, and $\lim_j \lim_i F(i, j)$ is the limit of this functor. (In more sophisticated language, there is a bijection between functors $I \times J \to C$ and functors $I \to C^J$. Since limits in functor categories are pointwise, if $\lim_i F(i, j)$ exists as a limit in $C$ for every $j \in J$ then $\lim_i F(i, {-})$ exists as a limit in $C^J$. So we can define $\lim_j \lim_i F(i, j)$ to be the limit of this resulting functor.)

## Ingredients

The two key ingredients in our proof are knowledge of how the Yoneda embedding interacts with limits, and the formula for limits in $\text{Set}$.

The Yoneda embedding $y : C \to \text{Set}^{C^\text{op}}$ preserves limits (by the universal property of limits) and reflects limits (since it is full and faithful). So if $G : I \to C$ is a diagram in $C$ then $\lim_i G(i)$ exists iff $\lim_i C({-}, G(i)) \cong C({-}, c)$ for some $c \in C$, in which case $c \cong \lim_i G(i)$.

Limits in $\text{Set}$ are particularly easy to handle. If $H : I \to \text{Set}$ is a diagram in $\text{Set}$ then we have
\[\lim_i H(i) = \Big\{\{x_i\} \in \prod_i H(i) \Big\vert H(f)(x_i) = x_{i'} \text{ for all $f : i \to i'$ in $I$}\Big\}\]
Note the RHS is the same as the set of cones over $H$ with summit the singleton set $1$ (since $\lim_i H(i) \cong \text{Set}(1, \lim_i H(i)) \cong \text{Cone}(1, H)$). Using the properties of limits in functor categories this also allows us to calculate limits in $\text{Set}^{C^\text{op}}$.

## Proof

The proof goes via the usual trick of using Yoneda to reduce to $\text{Set}$.

First let us prove the theorem in the case $C = \text{Set}$. Since $\text{Set}$ is complete, all (small) limits exist, and so if you want you're free to use the proof by uniqueness of adjoints I mentioned earlier. Here I instead give a very concrete proof using the explicit description of limits in $\text{Set}$, which I think is enlightening in a different way.

Let $F : I \times J \to \text{Set}$ a functor. Then for $j \in J$ we have
\[\lim_j F(i, j) = \Big\{\{x_j\} \in \prod_j F(i, j) \Big\vert F(i, g)(x_j) = x_{j'} \text{ for all $g : j \to j'$ in $J$}\Big\}\text{.}\]
Moreover given $f : i \to i'$ in $I$ we have an induced map $\lim_j F(f, j) : \lim_j F(i, j) \to \lim_j F(i', j)$ given by $\{x_j\} \mapsto \{F(f, j)(x_j)\}$. So we find
\begin{align*}
\lim_i \lim_j F(i, j) &= \Big\{\{y_i\} \in \prod_i \lim_j F(i, j) \Big\vert (\lim_j F(f, j))(y_i) = y_{i'}\\
&\quad\quad\text{ for all $f : i \to i'$ in $I$}\Big\}\\
&\cong \Big\{\{x_{i, j}\} \in \prod_{(i, j)} F(i, j) \Big\vert F(i, g)(x_{i, j}) = x_{i, j'} \text{ for all $g : j \to j'$ in $J$, }\\&\quad\quad F(f, j)(x_{i, j}) = x_{i', j} \text{ for all $f : i \to i'$ in $I$}\Big\}\\
&\cong \Big\{\{x_{i, j}\}  \in \prod_{(i, j)} F(i, j) \Big\vert F(f, g)(x_{i, j}) = x_{i', j'}\\
    &\quad\quad\text{ for all $(f, g) : (i, j) \to (i', j')$ in $I \times J$}\Big\}\\
&= \lim_{(i, j)} F(i, j)\\
\end{align*}
as desired.

Now consider the general case of a functor $F : I \times J \to C$, where we suppose $\lim_j F(i, j)$ exists for every $i \in I$. Applying the Yoneda embedding, we know that
\[C({-}, \lim_j F(i, j)) \cong \lim_j C({-}, F(i, j)) \text{.}\]
Now $\lim_i \lim_j F(i, j)$ exists and is (canonically) isomorphic to $c \in C$ iff there is a natural isomorphism
\begin{align*}C({-}, c) &\cong \lim_i C({-}, \lim_j F(i, j))\\
    &\cong  \lim_i \lim_j C({-}, F(i, j)) \text{.}
\end{align*}
But we know from the above special case that
\[\lim_i \lim_j C(c', F(i, j)) \cong \lim_{(i, j)} C(c', F(i, j))\]
for any $c' \in C$. Moreover it is straightforward to check that this isomorphism is natural in $c'$. So we find that
\[\lim_i \lim_j C({-}, F(i, j)) \cong \lim_{(i, j)} C({-}, F(i, j)) \text{.}\]
Now also $\lim_{(i, j)} F(i, j)$ exists and is isomorphic to $c \in C$ iff there exists a natural isomorphism
\[C({-}, c) \cong \lim_{(i, j)} C({-}, F(i, j)) \text{.}\]
So we deduce that $\lim_i \lim_j F(i, j)$ exists iff $\lim_{(i, j)} F(i, j)$ exists, in which case they are canonically isomorphic.
