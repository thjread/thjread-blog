----
title: The algebraic approach to tangent spaces of varieties
modified: 2020-03-06
meta_description: We define the Zariski tangent space of a variety in a geometric way, but we would like to connect this to our algebraic perspective. It turns out we can describe the tangent space just using the local ring at that point.
tags: Algebraic geometry, Part II
----

We define the (Zariski) tangent space of an irreducible affine variety in a very geometric way --- it is the zero set of linearised versions of the polynomials that define the variety. We want to connect this to the algebraic structures that we've been building up, like coordinate rings and function fields. In fact it turns out that there is a very nice way to view the tangent space at a point in terms of the local ring at at that point.

For some reason in the Cambridge Part II course we only prove this for curves --- I think the general case is much more enlightening. This proof is based on Hulek's Elementary Algebraic Geometry (Theorem 3.14), although somewhat expanded.

<!--more-->

## Motivation

Just like how we study varieties by looking at regular and rational functions, to take an algebraic approach to the tangent space we should look at the linear functionals on it --- that is, we should look at the dual vector space, known as the cotangent space. Intuitively (imagine a smooth manifold) we think of the tangent space at a point $p$ as looking like the variety near $p$, so a linear functional on the tangent space looks like the linear part of a function defined on the variety near $p$ (and since a linear functional is zero at the origin, we require our function to vanish at $p$). Translating this into more rigorous terminology, a "function defined on the variety near $p$" should mean a rational function which is regular at $p$, i.e. an element of the local ring $\mathcal{O}_{X, p}$ at $p$. The set of such functions which vanish at $p$ is the maximal ideal $m_p \triangleleft \mathcal{O}_{X, p}$. We want to consider just the linear part of these functions, so we quotient out by the set of functions which vanish to second order at $p$, i.e. $m_p^2$. So our aim now is to show
\[(T_{X, p})^\ast \cong m_p / m_p^2 \text{.}\]

## Preparation

In the proof, it will pay off to have thought carefully about how we can describe the tangent space and the dual space of the tangent space --- once we've done this, the rest is really just carefully manipulating definitions.

Given $f \in k[x_1, \dotsc, x_n]$, and a point $p \in \mathbb{A}^n_k$, we can define a linear functional corresponding to the linear part of $f$ at $p$

\[f^{\text{lin}}_p(v) = \sum_{i=1}^n v_i \diffp{f}{x_i}(p) \in (k^n)^\ast \text{.}\]

Now given an irreducible affine variety $X = \mathbb{V}(I) \subset \mathbb{A}^n_k$, the tangent space at $p \in X$ is defined as

\[T_{X, p} = \{v \in k^n \mid f^{\text{lin}}_p(v) = 0 \text{ for all $f \in I$}\}\text{.}\]

That is, if we let $I^\text{lin}_p = \{f^\text{lin}_p \mid f \in I\}$ then $I^\text{lin}_p$ is a subspace of $(k^n)^\ast$ (since $I$ is an ideal), and the tangent space is simply the annihilator of this subspace under the usual identification of a space and its double dual, i.e.

\[T_{X, p} = (I^\text{lin}_p)^\circ = \{v \in k^n \mid \theta(v) = 0 \text{ for all $\theta \in I^\text{lin}_p$}\}\text{.}\]

Now consider the cotangent space $(T_{X, p})^\ast$. Any linear functional $\theta$ on $k^n$ can be restricted to a functional on $T_{X, p}$, giving us a surjective linear map

\[\rho : (k^n)^\ast \to (T_{X, p})^\ast \text{.}\]

The kernel of this map is the annihilator of the tangent space, $(T_{x, p})^\circ$. But we have

\[(T_{x, p})^\circ = ((I^\text{lin}_p)^\circ)^\circ = I^\text{lin}_p\text{.}\]

So the cotangent space is $(k^n)^\ast$ quotiented by the linear parts of the polynomials in $I$,

\[(T_{X, p})^\ast \cong (k^n)^\ast / I^\text{lin}_p\text{.}\]

The key point here is the fact that the annihilator of the annihilator of a subspace is the original subspace, which tells us that going from the set of linear parts of polynomials in $I$ to the tangent space doesn't lose any information.

## Proof

Let $X = \mathbb{V}(I)$ an irreducible affine variety, and let $p = (a_1, \dotsc a_n) \in V$. Let $\mathcal{O}_{X, p}$ be the local ring at $p$, with maximal ideal $m_p$. Then we want to show that there is a natural isomorphism of vector spaces

\[(T_{X, p})^\ast \cong (m_p / m_p^2) \text{.}\]

We will start in the polynomial ring $k[x_1, \dotsc, x_n]$, and work our way down to $\mathcal{O}_{X, p}$.

Let $M_p = (x_1 - a_1, \dotsc, x_n - a_n) \triangleleft k[x_1, \dotsc, x_n]$ the maximal ideal of polynomials which are zero at $p$. We have a linear map taking polynomials to their linear part at $p$,

\begin{align*}
d : M_p &\to (k^n)^\ast\\
f &\mapsto f^{\text{lin}}_p \text{.}
\end{align*}

This map is surjective (since $d(x_i) = x_i$) and has kernel $M_p^2$, so induces an isomorphism $M_p / M_p^2 \cong (k^n)^\ast$. Recall the surjective map $\rho : (k^n)^\ast \to (T_{X, p})^\ast$ induced by restricting functionals from $k^n$ to $T_{X, p}$, with kernel $I^\text{lin}_p = \{f^{\text{lin}}_p \mid f \in I\}$. Composing $d$ and $\rho$ gives a surjective map

\[D : M_p \xrightarrow{d} (k^n)^\ast \xrightarrow{\rho} (T_{X, p})^\ast \text{.}\]

with kernel $d^{-1}(I^\text{lin}_p) = I + M_p^2$. Hence

\[(T_{X, p})^\ast \cong M_p / (M_p^2 + I)\text{.}\]

Consider the coordinate ring $k[X] = k[x_1, \dotsc, x_n]/I$. We have a corresponding maximal ideal $\overline{M}_p = M_p + I$, and we see that

\[(T_{X, p})^\ast \cong M_p / (M_p^2 + I) \cong \overline{M}_p / \overline{M}_p^2\text{.}\]

Now we're almost there --- we just need to show $\overline{M}_p / \overline{M}_p^2 \cong m_p / m_p^2$. The inclusion $\overline{M}_p \subseteq m_p$ (considering $k[X]$ and $\mathcal{O}_{X, p}$ as subrings of the function field $k(X)$) induces a map

\[\varphi : \overline{M}_p / \overline{M}_p^2 \to m_p / m_p^2\]

and we claim that this is an isomorphism. This is annoyingly fiddly to show --- Hulek implies that injectivity is obvious, but it isn't to me; let me know if you see a better approach!

For injectivity, take $f \in M_p$ and suppose $\varphi([\bar{f}]) = 0$. Then $\frac{f}{1} \in m_p^2$, so there exists a representation $\frac{f}{1} \sim \frac{f_1 f_2}{g}$ where $f_1, f_2, g \in k[x_1, \dotsc, x_n]$ with $f_1, f_2 \in M_p$ and $g \notin M_p$. This means that $f g - f_1 f_2 \in I$. But then $\bar{f} \bar{g} = \bar{f_1} \bar{f_2} \in \overline{M}_p^2$ as elements of $k[X]$. So it suffices to show that $\bar{g} \notin \overline{M}_p$ is invertible modulo $\overline{M}_p^2$. Consider the ideal $\overline{M}_p^2 + (\bar{g})$ -- we want to show that this is all of $k[X]$. But $\mathbb{V}(\overline{M}_p^2) = \{p\}$ and $p \notin \mathbb{V}(\bar{g})$, so $\mathbb{V}(\overline{M}_p^2 + (\bar{g})) = \emptyset$ and so by the Nullstellensatz the ideal must be the whole ring.

For surjectivity, take $f/g$ a representation for an element of $m_p$ --- that is, we have $f, g \in k[x_1, \dotsc, x_n]$ with $f \in M_p$ and $g \notin M_p$, so $c = g(p) \ne 0$. Then

\[\frac{f}{g} - \frac{f}{c} = \frac{f(c-g)}{gc} \in m_p^2\]

so $\varphi([c^{-1}\bar{f}]) = \left[\frac{f}{g}\right] \in m_p / m_p^2$. So $\varphi$ is surjective.

Hence $\overline{M}_p / \overline{M}_p^2 \cong m_p / m_p^2$ and we're done.

## Consequences

The most important benefit of this perspective on the tangent space is that it is coordinate independent. We can now define the tangent space just in terms of the local ring at a point; the local ring is defined in terms of the rational functions on the variety; and the rational functions only depend on the variety, not on how it is embedded in affine or projective space. In particular it is now easy to check that our definition of the tangent space of a projective variety doesn't depend on a choice of affine patch.

Avoiding coordinates also gives cleaner proofs of important properties of the tangent space, which otherwise require messy formulae and formal differentiation. For example suppose $f : X \dashrightarrow Y$ is a dominant rational map, regular at $p \in X$. Then precomposition by $f$ induces a homomorphism of function fields $f^\ast : k(Y) \to K(X)$. It is easy to see that $f^\ast$ sends rational functions which are regular at $f(p)$ to rational functions which are regular at $p$. So $f^\ast$ restricts to a homomorphism $\mathcal{O}_{Y, f(p)} \to \mathcal{O}_{X, p}$. Denote the maximal ideals by $n_{f(p)} \triangleleft \mathcal{O}_{Y, f(p)}$ and $m_p \triangleleft \mathcal{O}_{X, p}$, then $f^\ast$ further induces a linear map $n_{f(p)} / n_{f(p)}^2 \to m_p / m_p^2$. And this gives us a linear map $\mathrm{d}\!f_p : T_{X, p} \to T_{Y, f(p)}$. Moreover it is clear that $\mathrm{d}(g \circ f)_p = \mathrm{d}\!g_{f(p)} \circ \mathrm{d}\!f_p$ (the "chain rule").
