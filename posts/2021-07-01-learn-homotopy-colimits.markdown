----
title: How to learn about homotopy limits and colimits
modified: 2021-07-01
meta_description: TODO
tags: Homotopy theory, Model categories, Category theory
----

<!---
Preqrequisites: TODO. understand limits and colimits, basic topology. simplicial sets, nerves? coends?
-->

Earlier this year I wanted to quickly learn the basics of homotopy colimits, and found it surprisingly hard to even find a clear definition. Resources vary a lot in the level of generality, the amount of prerequisite knowledge required and the exact technical details, and it took a long time to find the right fit for me at a point when I didn't know what I was looking for. In this post I'll give a brief explanation of what homotopy colimits are, and point to the further resources that I found most helpful.

TODO do a drawing?

## What's the point of homotopy colimits?

In homotopy theory, we like to consider topological spaces ``up to homotopy equivalence''. In calculations, we want to be able to replace a space with a homotopy equivalent space. Unfortunately the usual category-theoretic notions of limit and colimit don't behave nicely with respect to homotopy equivalence. For example, the pullback of the diagram given by the inclusions of the endpoints in the unit interval

TODO diagram, make sure alt text is legible

is the empty topological space. On the other hand, the pullback of the diagram given by two inclusions of a point into a point

TODO diagram

is a point. But the interval is homotopy equivalent to the point -- in fact we have a commutative diagram

TODO diagram

where all the vertical maps are homotopy equivalences. Since the empty space is not homotopy equivalent to a point, we see that the limits of two homotopy equivalent diagrams are not necessarily homotopy equivalent.

<!--more-->

Homotopy limits fix this problem, at the expense of no longer satisfying the universal property of a limit (they do in fact satisfy a weaker "homotopy universal property", which we will discuss later TODO). Given a diagram
\[B \xrightarrow{f} A \xleftarrow{g} C \text{,}\]
the pullback is the space
\[\{(b, c) \in B \times C \mid f(b) = g(c) \text{,}\]
topologised as a subspace of $B \times C$. The homotopy pullback can be defined as the space
\[\{(b, c, \gamma) \in B \times A^I \times C \mid f(b) = \gamma(0), g(c) = \gamma(1)\} \text{,}\]
topologised as a subspace of $B \times C \times A^I$, where $A^I$ is the space of paths in $A$ and has the compact-open topology. The rough idea is that the equality constraint $f(b) = g(c)$ in the definition of pullback is badly behaved with respect to homotopy equivalence, so in the homotopy pullback we remove the equality constraint and instead add the data of a path from $f(b)$ to $g(c)$.

In our examples above, we can now compute that the homotopy pullback of $\{\ast\} \hookrightarrow \{\ast\} \hookleftarrow \{\ast\}$ is still a single point, while the homotopy pullback of $\{0\} \hookrightarrow [0, 1] \hookleftarrow \{1\}$ is the space of paths from $0$ to $1$ in $[0, 1]$, which is homotopic to a point.


## Key properties

- explain key properties (source), briefly explain construction (?)

## Definitions

## Further resources

- why is it hard? different definitions - Bousfield-Kan, simplicial replacement, other weird thing, two-sided bar construction. many prerequisites - model category stuff. does it need to be in a convenient category? which convenient category? some people cover general approach in model category, but important simplifications for Top. homotopy equivalence vs weak homotopy equivalence.
- resources: original source (explain about second part independent from first), beginners resources (see Zotero), something which covers two-sided bar construction, mention Riehl's book?
Try to ake prerequisites for each source clear. Point to something which describes what happens in the key cases: pushout, mapping telescope, pullback, walking arrow
homotopy universal property

## TODO conclusion

- end: point out that may be wrong, may be missing stuff, suggest people comment with corrections and/or resources

