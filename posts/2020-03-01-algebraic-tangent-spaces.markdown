---
title: The algebraic approach to tangent spaces of varieties
---

We define the tangent space of an irreducibile affine variety in a very geometric way -- ...

## Proof

Let $V = \mathbb{V}(I)$ an irreducible affine variety, and let $p \in V$. Let $\mathcal{O}_{V, p}$ be the local ring at $p$, with maximal ideal $m_p$. Then we claim that there is a natural isomorphism of vector spaces

\[T_{V, p} \cong (m_p / m_p^2) \text{.}\]

Let $M_p$ be the maximal ideal of $p$ in $\mathbb{A}^n_k$, and let $\bar{M}_p = M_p / I(V)$ the maximal ideal in $k[V]$.

The coordinate functions $x_1, \dotsc, x_n$ form a basis of $(k^n)^\ast$. For $f \in k[x_1, \dotsc, x_n]$ we have a corresponding linear functional

\[f^{(1)}_p = \sum_{i=1}^n \diffp{f}{x_i}(p) x_i \in (k^n)^\ast \text{.}\]

Hence we obtain a linear map

\[d : M_p \to (k^n)^\ast \quad f \mapsto f^{(1)}_p \text{.}\]

This map is surjective ($d(x_i) = x_i$) and has kernel $M_p^2$, so induces an isomorphism $M_p / M_p^2 \cong (k^n)^\ast$. The inclusion $T_{V, p} \subset k^n$ induces a surjection $(k^n)^\ast \to (T_{V, p})^\ast$ via restriction of functionals, so we get a surjection

\[D : M_p / M_p^2 \to (k^n)^\ast \to (T_{V, p})^\ast \text{.}\]

Now we have

\begin{align*}f \in \ker(D) &\iff f^{(1)}_p \!\mid_{T_{V, p}}\\
&\iff f^{(1)}_p = g_{p}^{(1)} \quad \text{for some $g \in I(V)$}\\
&\iff f - g \in M_p^2  \quad \text{for some $g \in I(V)$}\\
&\iff f \in M_p^2 + I(V)
\end{align*}

(where the second step follows by the fact that $T_{V, p} \cong \{d(g) \mid g \in I(V)\}^\circ \subset k^n$ so $f^{(1)}_p \!\mid_{T_{V, p}} = 0$ iff $f \in (T_{V, p})^\circ \cong \text{span}\{d(g) \mid g \in I(V)\} = \{d(g) \mid g \in I(V)\} \subset (k^n)^\ast$).

Hence we have

\[\bar{M}_p / \bar{M}_p^2 \cong M_p / (M_p^2 + I(V)) \cong (T_{V, p})^\ast \text{.}\]

So it suffices to show that $\bar{M}_p / \bar{M}_p^2 \cong m_p / m_p^2$. The inclusion $\bar{M}_p \to m_p$ induces an inclusion

\[\varphi : \bar{M}_p / \bar{M}_p^2 \to m_p / m_p^2\]

so suffices to check that $\varphi$ is surjective. Take $f/g \in m_p$, and let $c = g(p) \ne 0$. Then

\[\frac{f}{g} - \frac{f}{c} = \frac{f(c-g)}{gc} \in m_p^2\]

so $\varphi(f/c) = \overline{(f/g)} \in m_p / m_p^2$.

