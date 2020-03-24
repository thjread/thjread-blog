----
title: Getting started with category theory
modified: 2020-03-24
meta_description: How should you learn category theory? This is a reading list of resources, based on my experience learning category theory over the past six months.
tags: Category theory
----

So you want to learn category theory - where should you start? I've spent the past six months trying to get a background in the essentials, and so it's a good time to write up a reading list to point others along the same path. This is more of a record of my journey than an exhaustive guide, although I'll try to mention all the popular resources I know of.

## Introductory resources

### Physics, Topology, Logic and Computation: A Rosetta Stone --- John Baez and Michael Stay

If you're like me, you'll want to get excited about the subject before wading through a couple hundred pages of textbook. This paper is perfect for that --- it describes how category theory draws beautiful analogies between different areas of physics, maths and computer science, all assuming no prerequisites.

The [paper is available from John Baez's website](http://math.ucr.edu/home/baez/rosetta/rose3.pdf).

### Category Theory for Programmers --- Bartosz Milewski 

The first part of this is one of the best gentle introductions to category theory that I know of. He tries to avoid too much mathematical notation and technical detail, and instead gives intuition with cute drawings and keeps things grounded with snippets of Haskell code. If you have a formal training in mathematics you might prefer the next item on the list; on the other hand if you want to learn category theory in order to understand Haskell then this might be the only thing you need to read.

The [posts are online here](https://bartoszmilewski.com/2014/10/28/category-theory-for-programmers-the-preface/) --- that page also links to a pdf version, a hardcover book and a series of YouTube videos.

### Category Theory in Context --- Emily Riehl

This was my choice of introductory category theory textbook. It's accessible to a later year maths undergrad without needing to know an excessive amount of abstract algebra or other prerequisites, and the writing is clear and of a consistently high quality. The content is evidently very well thought through: every time I skipped something I would find that it became important later, and need to go back and learn it properly! It's a fairly dense book but not needlessly so --- all the key intuition and examples are there.

You can [read the book for free online](http://www.math.jhu.edu/~eriehl/context.pdf), and the paperback version is inexpensive.

<!--more-->

### The Catsters' Category Theory Videos --- Eugenia  Cheng  and  Simon  Willerton

Short YouTube videos covering a variety of topics from basic to quite advanced. A good way to fill in gaps in your knowledge, and somewhat addictive. I especially recommend the videos on string diagrams, since they are much easier to understand when someone draws them in front of you (and aren't covered in many of the textbooks).

There's an [organised collection of videos here](http://www.simonwillerton.staff.shef.ac.uk/TheCatsters/).

## Honourable mentions

Here are a few more sources which I would recommend, although I have less personal experience with these.

### Basic Category Theory --- Tom Leinster

This covers a bit less than Category Theory in Context, though still most of the important basics. I haven't read it, but I suspect this is a good choice, especially if you want something a little lighter.

The [book is online here](https://arxiv.org/pdf/1612.09375.pdf); there's also a somewhat expensive hardback version.

### Category Theory --- Steve Awodey

Another similar book, especially aimed at those with less of a maths background who e.g. might not know what a group is.

The [book is available online](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.211.4754&rep=rep1&type=pdf), or there's a not overly expensive paperback.

### Categories for the Working Mathematician --- Saunders Mac Lane

The original and most famous textbook. Harder than Category Theory in Context, although still fairly readable if you have sufficient mathematical background. Covers slightly more content.

The book seems to be fairly expensive. Or if you google it you may or may not come across a pdf...

### Seven Sketches in Compositionality: An Invitation to Applied Category Theory --- Brendan Fong and David Spivak

I've only read the first couple chapters but enjoyed them. Introductory, but with a very different syllabus from the other textbooks, focusing on the aspects used most in applied category theory such as enriched categories and monoidal categories. Each chapter develops a topic in the context of a concrete example.

You can [read it online](https://math.mit.edu/~dspivak/teaching/sp18/7Sketches.pdf), or you can buy a paperback copy.

### Math3ma

Great blog posts on a variety of ideas from category theory. Especially good for explaining what a concept really means and how you should think about it.

See [https://www.math3ma.com/](https://www.math3ma.com/) --- in particular [posts tagged "category theory"](https://www.math3ma.com/categories/category-theory).

## Next steps

Now that you know the basics, where should you go next? Of course this will depend on what you're interested in and why you're learning category theory --- here is just a selection of things that I've enjoyed or that have been recommended to me.

### Coend calculus --- Fosco Loregian

(Co)end calculus is a very useful calculational tool, and eventually you'll run into enough ends and coends that you'll need to learn how they work. In particular they make working with Kan extensions much easier --- if you read Category Theory in Context then this will make Chapter 6 make much more sense. The first two chapters of this book cover all the key properties, and each chapter comes with an exhaustive set of exercises.

A word of advice --- don't worry too much about understanding dinatural transformations. All the guides on (co)ends that I've read include a section on them, but as long as you know the definition of a (co)wedge then that's all you will need.

The [paper is available from the arXiv](https://arxiv.org/abs/1501.02503).

### 2-Dimensional Categories --- Niles Johnson and Donald Yau

I've only just started reading this, but it got a fair amount of publicity when it was published recently. My understanding is that the book collects together a variety of material that was previously mostly scattered across various papers without many accessible introductions. In particular they include precise statements and detailed proofs of many results which are left to the reader elsewhere.

The [book is on the arXiv](https://arxiv.org/abs/2002.06055).

### Sheaves in Geometry and Logic: A First Introduction to Topos Theory --- Saunders Mac Lane and Ieke Moerdijk

Another book that I've only recently started, but one which several people have recommended to me. 

Note that the chapters on sheaves in the context of geometry are largely independent from those on elementary topoi and logic, so don't feel like you need to read the whole thing in order.

Quite expensive to buy --- again there seem to be a few pdfs floating around on the web.

### The $n$Lab

No list of category theory resources would be complete without mentioning the $n$Lab --- a vast wiki with content varying from pure category theory, to categorical perspectives on other areas of maths, to random unrelated bits of maths. Invaluable, and usually the first stop for tracking down an unknown concept or definition.

See [https://ncatlab.org](https://ncatlab.org).

&nbsp;

Let me know in the comments if there's anything I should add!
