----
title: "Differentiating vector fields: covariant derivatives, parallel transport and connections"
modified: 2020-06-18
meta_description: TODO
tags: Differential geometry, General relativity
----

How can you differentiate a vector field on a smooth manifold? This post aims to give an intuitive explanation of the answer, describing it from several different viewpoints. It turns out that if you have a (pseudo-)Riemannian manifold (i.e.\ you have a way of measuring lengths and angles) then there is a canonical way to differentiate a vector field, called the *Levi-Civita connection*. In the general setting there isn't a canonical choice, but we can describe the family of possible ways to equip a manifold with the ability to differentiate. I'll largely focus on the Riemannian manifold case in this post.

## Background and setup

<!-- TODO diagram? -->

Consider a smooth Riemannian manifold $M$, and a vector field $X$ on $M$. That is, for each point $p \in M$ we choose a vector $X(p) \in T_p M$ from the tangent space at $p$, such that this vector "varies smoothly" depending on $p$. For example we might model the surface of the Earth by a sphere, which is a smooth manifold, and then consider the vector field given by the magnitude and direction of the wind at each point on the Earth's surface.

Now we want to investigate how $X$ changes as we change $p$. For example given a point in $M$ and a direction in the tangent space, we might want to calculate the rate of change of $X$ as we move from that point in that direction. If we consider $M$ as a manifold embedded in $\mathbb{R}^n$ then this is easy - we can identify all the tangent spaces with vector subspaces of $\mathbb{R}^n$, and so consider $X$ as a smooth function $M \to \mathbb{R}^n$. And now we can calculate the usual directional derivative.

But in differential geometry we like our constructions to be *intrinsic* -- they should not vary depending on a particular embedding or choice of coordinate system. In general relativity we need to deal with a curved 4-dimensional spacetime which has no preferred embedding into Euclidean space.

So we need to come up with a new approach. The obvious choice is to have the derivative take values in the tangent space, since there's no intrinsic analogue of $\mathbb{R}^n$ above. But this means we really do need something genuinely different, because in the embedded case the directional derivative does not always lie in the tangent space.

<!-- TODO diagram? -->

<!--more-->

## First steps

Let's go back to the definition of the directional derivative. Given a smooth function $F : \mathbb{R}^n \to \mathbb{R}^m$, a point $x \in \mathbb{R}^n$ and a direction $v \in \mathbb{R}^n$, we define the directional derivative as a limit
\[\nabla_v F(x) = \lim_{t \to 0} \frac{F(x + tv) - F(x)}{t} \text{.}\]
Now go back to our manifold $M$ and vector field $X$. We don't (yet!) have a way to move along $M$ in a particular direction, so in place of considering the value of $F$ at $x + tv$, we consider the value of $X$ along a smooth path $\gamma : \mathbb{R} \to M$ with $\gamma(0) = p \in M$. Now we have something like this:
\[\lim_{t \to 0} \frac{X(\gamma(t)) - X(\gamma(0))}{t} \text{.}\]
But $X(\gamma(t)) \in T_{\gamma(t)}M$ (the tangent space at $\gamma(t)$ and $X(\gamma(0)) \in T_{\gamma(0)}M$ (the tangent space at $\gamma(0)$), so we can't take the difference of these two vectors. In order to make something like this work we'll need to find a way to "transport" a vector from $T_{\gamma(t)}M$ to $T_{\gamma(0)}M$.

## Parallel transport

This idea of connecting the tangent spaces at different points on the manifold is called *parallel transport*. The most important thing to understand about parallel transport is that if you have a tangent vector at one point, and you want to produce a corresponding tangent vector at a second point, you need to transport it along a specific path from the first point to the second point. If you transport the vector along a different path you'll get a different result -- in fact the difference measures the *curvature* of the manifold.

<!-- diagram -->

That probably didn't make much sense at first reading, so let's consider an example. Imagine a sphere, with a short arrow drawn on the surface at some point on the equator, pointing north. Now imagine moving the arrow to the north pole, either by dragging it straight up a line of longitude, or by first moving it along the equator for a bit before heading up to the north pole. Then you see that the arrow ends up pointing in a different direction depending on the path you took! This is because the sphere is curved. On the other hand if you drag an arrow around on a flat plane then it will always remain parallel (in the usual sense), regardless of the path you take.

This example relied on your intuitive notion of how you can drag around an arrow on a surface, but this can be made precise for any Riemannian manifold. One idea is to take the tangent plane, and "roll" it along the surface so that the point of tangency traces out the desired path. A more modern approach is to first define the covariant derivative, and define parallel transport in terms of that.

## Covariant derivative

- parallel transport by rolling tangent planes
- covariant derivative by differentiating embedded and then projecting
- Levi-Civita connection, (Pseudo)-Riemannian manifold

Brief description of manifold, Riemannian manifold, tangent space, vector field, tangent bundle.
Motivation: usual limit def of directional derivative, need parallel transport to be well-defined
Parallel transport on sphere example

Parallel transport from covariant derivative
Connection in terms of vertical and horizontal subbundles of tangent bundle?
