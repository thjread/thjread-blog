---
title: The algebraic approach to tangent spaces of varieties
---

We define the tangent space of an irreducible affine variety in a very geometric way -- it is the zero set of linearised versions of the polynomials that define the variety. We want to connect this to the algebraic structures that we've been building up, like coordinate rings and function fields. In fact it turns out that there is a very nice way to view the tangent space at a point in terms of the local ring at at that point.

For some reason in the Cambridge Part II course we only prove this for curves -- I think the general proof is much more enlightening, and not really much harder.

## Motivation

Just like how we study varieties by looking at regular and rational functions on them, to take an algebraic approach to the tangent space we should look at the linear functionals on it -- that is, we should look at the dual vector space, known as the cotangent space. Intuitively (imagine a smooth manifold) we think of the tangent space at a point $p$ as looking like the variety near $p$, so a linear functional on the tangent space looks like the linear part of a function defined on the variety near $p$ (and since a linear functional is zero at the origin, we require our function to vanish at $p$). Translating this into more rigorous terminology, a "function defined on the variety near $p$" should mean a rational function which is regular at $p$, i.e. an element of the local ring $\mathcal{O}_{X, p}$ at $p$. The set of such functions which vanish at $p$ is the maximal ideal $m_p \triangleleft \mathcal{O}_{X, p}$. We want to consider just the linear part of these functions, so we quotient out by the set of functions which vanish to second order at $p$, i.e. $m_p^2$. So our aim now is to show
\[(T_{X, p})^\ast \cong m_p / m_p^2 \text{.}\]

## Preparation

In the proof, it will pay off to have thought carefully about how we can describe the tangent space and the dual space of the tangent space -- once we've done this, the rest is really just carefully manipulating definitions.

Given $f \in k[x_1, \dotsc, x_n]$, and a point $p \in \mathbb{A}^n_k$, we can define a linear functional corresponding to the linear part of $f$ at $p$

\[f^{\text{lin}}_p(v) = \sum_{i=1}^n v_i \diffp{f}{x_i}(p) \in (k^n)^\ast \text{.}\]

Now given an irreducible affine variety $X = \mathbb{V}(I) \subset \mathbb{A}^n_k$, the tangent space at $p \in X$ is defined as

\[T_{X, p} = \{v \in k^n \mid f^{\text{lin}}_p(v) = 0 \text{ for all $f \in I$}\}\text{.}\]

That is, if we let $A = \{f^\text{lin}_p \mid f \in I\}$ then $A$ is a subspace of $(k^n)^\ast$ (since $I$ is an ideal), and $T_{X, p}$ is simply the annihilator (under the usual identification of a space and its double dual) $A^\circ = \{v \in k^n \mid \theta(v) = 0 \text{ for all $\theta \in A$}\}$.

Now consider the cotangent space $(T_{X, p})^\ast$. Any linear functional $\theta$ on $k^n$ can be restricted to a functional on $T_{X, p}$, giving us a surjective linear map

\[\rho : (k^n)^\ast \to (T_{X, p})^\ast \text{.}\]

The kernel of this map is the annihilator of the tangent space, $(T_{x, p})^\circ$. But we have $(T_{x, p})^\circ = (A^\circ)^\circ = A$! So the cotangent space is $(k^n)^\ast$ quotiented by the linear parts of the polynomials in $I$, $(T_{X, p})^\ast \cong (k^n)^\ast / A$.

The key point here is the fact that the annihilator of the annihilator of a subspace is the original subspace, which tells us that going from the set of linear parts of polynomials in $I$ to the tangent space doesn't lose any information.

## Proof

Let $X = \mathbb{V}(I)$ an irreducible affine variety, and let $p = (a_1, \dotsc a_n) \in V$. Let $\mathcal{O}_{X, p}$ be the local ring at $p$, with maximal ideal $m_p$. Then we claim that there is a natural isomorphism of vector spaces

\[(T_{X, p})^\ast \cong (m_p / m_p^2) \text{.}\]

We will start in the polynomial ring $k[x_1, \dotsc, x_n]$, and work our way down to $\mathcal{O}_{X, p}$. Let $M_p = (x_1 - a_1, \dotsc, x_n - a_n)$ the maximal ideal of polynomials which are zero at $p$.

We have a linear map taking polynomials to their linear part at $p$,
\[d : M_p \to (k^n)^\ast \quad\quad f \mapsto f^{\text{lin}}_p \text{.}\]

This map is surjective ($d(x_i) = x_i$) and has kernel $M_p^2$, so induces an isomorphism $M_p / M_p^2 \cong (k^n)^\ast$. As we discussed earlier, restriction of functionals gives a surjection $\rho : (k^n)^\ast \to (T_{X, p})^\ast$, giving us a surjective map

\[D : M_p / M_p^2 \xrightarrow{d} (k^n)^\ast \xrightarrow{\rho} (T_{X, p})^\ast \text{.}\]

The kernel of $\rho$ is $A = \{f^{\text{lin}}_p \mid f \in I\}$, so $D$ has kernel $I$ (mod $M_p^2$). That is,
\[(T_{X, p})^\ast \cong M_p / (M_p^2 + I)\text{.}\]

Now consider the coordinate ring $k[X] = k[x_1, \dotsc, x_n]/I$. We have a corresponding maximal ideal $\overline{M}_p = M_p + I$, and we see that
\[(T_{X, p})^\ast \cong \overline{M}_p / \overline{M}_p^2\text{.}\]

Now we're almost there -- we just need to show $\overline{M}_p / \overline{M}_p^2 \cong m_p / m_p^2$. The inclusion $\overline{M}_p \subseteq m_p$ (considering $k[X]$ and $\mathcal{O}_{X, p}$ as subrings of the function field $k(X)$) induces an injection

\[\varphi : \overline{M}_p / \overline{M}_p^2 \to m_p / m_p^2\]

so it suffices to check that $\varphi$ is surjective. Take $f/g$ a representation for an element of $m_p$ -- that is, we have $f, g \in k[X]$, $f(p) = 0$, and $c = g(p) \ne 0$. Then

\[\frac{f}{g} - \frac{f}{c} = \frac{f(c-g)}{gc} \in m_p^2\]

so $\varphi(f/c) = \overline{(f/g)} \in m_p / m_p^2$, $\varphi$ surjective. So we're done.

## Consequences

TODO

TODO credit Hulek

TODO be more careful about $\varphi$ injective?
