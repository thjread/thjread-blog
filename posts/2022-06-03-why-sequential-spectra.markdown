----
title: Understanding sequential spectra
modified: 2022-06-03
meta_description: TODO
tags: Homotopy theory, Model categories
prerequisites: homotopy theory (higher homotopy groups), model categories (basic concepts)
----

- briefly motivate stable htpy theory, omega-spectra
- explain role of sequential spectra in studying htpy theory of omega-spectra

## Introduction

- link to nlab https://ncatlab.org/nlab/show/Introduction+to+Stable+Homotopy+Theory

Stable homotopy theory can be tricky to motivate, in part because there are lots of different sources of motivation, each requiring different prerequisite knowledge to understand. No one of these is a slam-dunk reason to study stable homotopy theory, but the combination of all of them makes it clear that it's a very natural piece of mathematics.

## Omega-spectra

- name drop Brown representability, Freudenthal suspension
- look at the things Dhruva sent?
- but perhaps just motivate as allowing for negative htpy groups / inverting loops operation

## Sequential spectra and the stable model structure

Suppose we agree that we want to study Omega-spectra. The next step is to try to do homotopy theory, replacing spaces with Omega-spectra.

What exactly do we mean by "do homotopy theory"? A truly satisfactory answer would probably involve $\infty$-categories, but I don't know enough to do justice to that point of view. Instead we'll use model categories to gain access to the homotopy theory of spectra.

The basic idea motivating the theory of model categories is to take the tools we use to study the homotopy theory of spaces, and abstract them so that they can be applied to other scenarios. A [model category](https://ncatlab.org/nlab/show/model+category#definition) is a complete and cocomplete category with three specified classes of morphisms called weak equivalences, fibrations and cofibrantions (analagous to weak homotopy equivalences, Serre fibrations and retracts of relative cell complexes), satisfying certain factorisation and lifting properties. It turns out that this is enough to define a notion of homotopy of maps, and to prove a version of the Whitehead theorem.

This allows you to study the homotopy category -- the result of formally inverting all the weak equivalences. A more explicit way to construct the homotopy category is that its objects are the fibrant and cofibrant[^fibrant_cofibrant] objects of the model category, and its morphisms are homotopy classes of maps between the objects. For topological spaces, the homotopy category is equivalent to the category of CW-complexes and homotopy classes of maps between them.

[^fibrant_cofibrant]: We say an object $X$ of a model category is fibrant if the map to the terminal object $X \to 1$ is a fibration, and cofibrant if the map from the initial object $\emptyset \to X$ is a cofibration. In the (Quillen) model structure on topological spaces, every space is fibrant, and the cofibrant spaces are the retracts of CW-complexes.

Back to Omega-spectra. The obvious approach would be to define a model category of Omega-spectra. Unfortunately this is a complete non-starter. There is an obvious category of Omega-spectra (where maps between Omega-spectra are collections of maps between the component spaces that commute with the structure maps), but this category isn't even complete, so there's no way to give it a model structure. <!-- TODO prove this! -->

Instead we take a step back. An Omega-spectrum is a structure (a collection of spaces and maps) satisfying some properties (certain maps are weak homotopy equivalences). Perhaps it would be useful to first study the structure without the properties (if you're a programmer, you might like to think of this as the underlying "datatype" -- the "shape" of the data). This gives the notion of a sequential spectrum.

::: definition
**Definition** (Sequential spectrum)

A sequential spectrum $X$ is a sequence of pointed topological spaces $X_n$, together with maps
\[X_n \to \Omega X_{n+1}\]
from each space to the loop space of the next.

(This definition is usually given in terms of maps $\Sigma X_n \to X_{n+1}$ out of the reduced suspension, but the two perspectives are equivalent by the loops-suspension adjunction.)
:::

We can define a category $\text{SeqSpec}$ of sequential spectra, where a map $X \to Y$ of sequential spectra is a collection of continuous maps $X_n \to Y_n$ that commute with the structure maps. <!-- TODO should probably write this more explicitly in the definition, will need commuting square diagram --> This time we do get a complete and cocomplete category (limits and colimits can both be computed objectwise). Even better, it turns out there's a natural model structure on $\text{SeqSpec}$, called the strict model structure on sequential spectra[^strict_model].

[^strict_model]: Under certain conditions, there's a natural model structure on the category of functors from a small category to a model category, called the projective model structure. It turns out that $\text{SeqSpec}$ is equivalent to a category of topologically enriched functors into the category of pointed spaces -- see [this section of the nLab guide](https://ncatlab.org/nlab/show/Introduction+to+Stable+homotopy+theory+--+1-1#TopologicalDiagramsSequentialSpectra) for an explanation. The strict model structure on sequential spectra is the projective model structure on this category of functors.

We've now made significant progress. We can define a homotopy category of Omega-spectra by considering the homotopy category of the strict model structure on sequential spectra, and taking the full subcategory spanned by the Omega-spectra. This will turn out to be the right things to study (it's usually called the "stable homotopy category"), but at the moment it's hard to compute with -- since the subcategory of Omega-spectra isn't closed under limits/colimits, it's hard to define operations that combine Omega-spectra and give new Omega-spectra.

The key is that a model category really describes the homotopy theory of the *fibrant-cofibrant* objects. Every object is weakly equivalent to a fibrant-cofibrant object called its fibrant-cofibrant replacement, and for homotopical purposes it functions as a stand-in for that object. So we need to construct a model category where all the fibrant-cofibrant objects are Omega-spectra. For this, we use something called (left) Bousfield localisation. It turns out that if we can define a new model structure on $\text{SeqSpec}$ with the same cofibrations but more weak equivalences (this is called a left Bousfield localisation) then the homotopy category associated to the new model structure will be a full (in fact reflective) subcategory of the homotopy category associated to the old model structure. And indeed there is such a model structure on $\text{SeqSpec}$ such that the homotopy category is precisely the subcategory spanned by the Omega-spectra[^omega-spectrification]. We call this the stable model structure on sequential spectra. As we hoped, all the fibrant-cofibrant objects are Omega-spectra. Even better, the Omega-spectra are precisely the fibrant objects under the stable model structure!

[^omega-spectrification]: The theory of Bousfield localisations gives some help in doing this. The most important ingredient is to define an "Omega-spectrification" functor that takes a sequential spectrum and approximates it by an Omega-spectrum.

TODO - another nice property of sequential spectra: get suspension spectrum?

## Further study

In the story so far, we've successfully constructed the stable homotopy category, and a useful model category to use to study it. This is already a good setting for a lot of stable homotopy theory. However it's not the end of the story. The stable homotopy category is in fact a symmetric monoidal category, with a monoidal product called the smash product of spectra. It is possible to define the smash product at the level of sequential spectra, but it doesn't have good properties until you pass to the homotopy category. This lead to the search for a new model category of spectra that has the same homotopy category, but where the symmetric monoidal structure of the homotopy category comes from a symmetric monoidal structure at the 1-categorical level. There are various solutions to this problem, such as symmetric spectra and orthogonal spectra, but I'll leave discussing these for another day! A well-behaved smash product of spectra opens up whole new areas of stable homotopy theory; in particular the so-called "brave new algebra".

<!--more-->

