----
title: TODO
modified: 2021-12-04
meta_description: TODO
tags: cooking, chemistry
prerequisites: TODO
----

I've recently been learning to cook Japanese food. While doing this, I discovered that kombu, a common Japanese ingredient, contains extremely levels of iodine. This post introduces kombu, discusses whether too much iodine is something to worry about or not, and describes my efforts to measure iodine levels at home. 

Disclaimer: I'm not a doctor or a chemist, so please do you own research before trying to replicate my experiments, and talk to your doctor if you have medical conditions that may interact with iodine intake!

TODO photo of something nice

## TL;DR

Kombu kelp has very high levels of iodine, and many recipes found on the internet contain much more iodine than is recommended by Western health agencies. The effects of high iodine intake are poorly understood. If you're healthy it might not matter, but if you are pregnant, breast-feeding or have an underlying thyroid condition then it can cause serious problems. If you're worried about this, you can remove most of the iodine from kombu by boiling it for 5 minutes or soaking it for 30 minutes, then discarding the water.


<!--more-->

## What is kombu?

![Left: a piece of dried kombu. Right: an equal sized piece after rehydrating.](/images/kombu_soak.jpg){.image-large .image-pad-vertical .center alt="Dry and reconstituted kombu on a plate"}

Kombu is a kind of kelp seaweed that grows in long, thick strips. It's frequently used in Japanese cooking: dried kombu is soaked in water to produce a form of stock called kombu dashi. This provides the base for miso soup as well as many other broths, stews and sauces.

In 1908, Japanese biochemist Kikunae Ikeda discovered that the distinctive umami taste of kombu comes from glutamic acid. He realised that this could be manufactured commercially in the form of monosodium glutamate (MSG). These days many Japanese cooks buy dashi powder rather than making homemade stock; most powders have added MSG, and some don't even contain any kombu.

In the west, kombu is becoming increasingly popular in vegan and vegetarian cooking. It's often added to herbal supplements and vegan milk substitutes as a rich natural source of iodine.

## Too much of a good thing?

Many seaweeds are good sources of iodine, but kombu and related seaweeds are the iodine champions. One gram of kombu (enough to make just 100ml of dashi) might contain 2-4 mg of iodine. A bowl of noodles served in a dashi-based broth might have 10 mg. Compare this to the recommended daily allowance of just 0.14 mg -- that's a lot of iodine!

Is that too much? It's hard to say. The UK's National Health Service recommends adults consume at most 0.5 mg of iodine per day, while the National Institutes of Health in the US list the "tolerable upper intake" level at 1.1 mg, and the Japanese Ministry of Health, Labour and Welfare gives a safe upper limit of 3 mg.

Iodine is used for the production of key thyroid hormones.
The thyroid tries to regulate its uptake of iodine so that it produces an appropriate quantity of hormones, regardless of how much iodine is in your food. However in those with thyroid conditions or previous iodine deficiency, excessive iodine consumption can cause this regulatory system to break down. Confusingly this can lead to the thyroid absorbing either too much or too little iodine, meaning that consuming an excessive amount of iodine can cause either overproduction or underproduction of thyroid hormones (hyperthyroidism or hypothyroidism).

I had a look for real world examples of iodine toxicity, and came across the case of Bonsoy soy milk in the late 2000s. Australian doctors noticed a cluster of patients with thyroid dysfunction, and identified the soy milk as a culprit. At the time Bonsoy was flavoured with kombu, and it had a similar iodine concentration to kombu dashi (about 30mg per litre). There was a class action lawsuit settled for $25m, divided among 500 people who had been harmed by the milk. Participants in the lawsuit described a wide variety of issues, including severe hyperthyroidism in a woman who consumed three to four glasses of the milk per day, and hypothyroidism in the newborn baby of a woman who drank 500ml per day.

This example shows that consuming a large (but not unrealistic) amount of dashi can cause serious problems for some people.
On the other hand, Japanese people eat a lot of kombu, generally without problems. [This paper](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3204293/) finds that the average person in Japan consumes about 1 mg-3 mg iodine per day, mostly from kombu -- presumably this corresponds to some people eating much less while others have much more. There are certainly some reports of iodine-induced thyroid problems in Japan, but not enough for people to be particularly concerned.

One complicating factor is that certain substances called goitrogens inhibit iodine uptake by the thyroid, or inhibit production of thyroid hormones. Some common goitrogens are tofu and other soy products; green tea; and cruciferous vegetables such as cabbage, brocolli and pak choi. These are often part of a Japanese diet, and so consuming kombu as part of a goitrogen-rich Japanese diet might have less effect than consuming it as part of an otherwise Western diet.

So far I've been talking interchangeably about the quantity of iodine in kombu seaweed itself, and the quantity in the resulting dashi stock. Surprisingly these are basically the same - it doesn't take long to soak out almost all of the iodine from a piece of kombu. According to [a Japanese paper from 1989](https://pubmed.ncbi.nlm.nih.gov/2744193/), 99% of the iodine ends up in the water if you boil kombu for 15 minutes, and 98% if you soak kombu at room temperature for 3 hours.

This suggests that you could pre-soak kombu to reduce the amount of iodine that ends up in the final dashi. Traditional Japanese cooking already features the idea of making dashi twice from the same piece of seaweed. The first, stronger dashi is called "ichiban dashi", and used for recipes where the dashi is a major ingredients, such as clear soups. The dashi you get when you re-use the same seaweed is called "niban dashi", and is used for stews and sauces. So hopefully a brief soak would remove most of the iodine while leaving behind enough flavour to make good dashi.

## Measuring iodine at home

I wanted to double-check the numbers from the 1989 paper and try different timings and temperatures, so I started looking into ways I could measure iodine in food. You can send a sample to a lab but it costs several hundred pounds -- definitely out of my budget for this project, so I'd have to do it myself. I needed to be able to detect iodine at the concentration I was expecting in dashi, about 30 mg per litre. That's a pretty small quantity, but not invisibly so: 30 mg is about half the mass of a grain of salt.

At school we learnt that you can detect iodine using the starch-iodine test. Pour an iodine solution onto something containing starch, like a cut piece of potato, and it'll stain the starch a deep blue-black. We usually used this as a test for starch, but it also works to test for iodine.

I tried putting a couple drops of dashi on some flour and ... nothing happened. The problem is that iodine is very reactive, and so dashi doesn't contain iodine in its elemental form. Instead it's likely to be present as iodide ions[^1].

[^1]: I couldn't find any information about the forms of iodine in seaweed specifically, but decided to work under the assumption that the iodine is mostly present in the form of iodide ions. It's probably also present in smaller quantities in iodate ions, or in compounds such as iodooctane.

To use the starch-iodine test, we need to convert those iodide ions into iodine. At this point I was reaching the limits of my rust chemistry knowledge, but luckily there's a famous experiment involving exactly the reaction we want, called the iodine clock reaction. You start with a solution of sulfuric acid and hydrogen peroxide, and add starch, sodium thiosulfate and potassium iodide. The hydrogen peroxide oxidises iodide to iodine, but the sodium thiosulfate converts the iodine back to iodide, acting too quickly for a significant concentration of iodine to build up. When the sodium thiosulfate runs out, the iodine can now remain in solution and form a complex with the starch, dramatically turning the solution a blue-black colour.

- chemical equation?

For our purposes we can ditch the sodium thiosulfate -- we just want to use the fact that under acidic conditions, hydrogen peroxide will convert iodide to iodine. Hydrogen peroxide is easy to find, since it's used as a household cleaner and disinfectant. I ordered some food-grade 3% hydrogen peroxide off the internet: food-grade means there are no additives that might interfere with the reaction, and 3% is sufficiently dilute that it's pretty safe. The sulfuric acid used in the iodine clock reaction is harder to get hold of, but any acid will do. Distilled vinegar works just fine.

To make the experiment somewhat quantitative, I also bought some pure potassium iodide for comparison. This came as a small bag of white powder from "The Pharmer's Market", which must be one of the most suspicious looking things I've ever ordered off the internet.

## Experimental results

TODO some sort of picture of experiments?

Somewhat to my surprise, the reaction worked exactly as hoped. A full description of what I did is detailed below in [Appendix: Experimental Details](#appendix-experimental-details). In short, I found that my kombu dashi does indeed contain a lot of iodine, about the level that I'd expected based on my research. I tried simmering a piece of kombu for five minutes, discarding the water, then soaking overnight. This led to a dashi with a low enough level of iodine that I couldn't measure it (less than 0.3 mg/L). A similar process where I soaked the kombu at room temperature for 30 minutes then discarded the water led to a dashi with about 0.5 mg/L -- still within the NHS's safe limit even if you drink a litre of it every day.

## Taste tests

Finally I needed to check how this process impacted the taste of the dashi! I anticipated that I might lose a lot of the highly soluble MSG along with the iodine; but that wouldn't be a big problem since I could just add MSG to replace it. Some people like kombu because they don't want to add artificial MSG; personally I'm perfectly happy adding MSG, but I also like the complex seaweed aromas found in traditional dashi. Kombu dashi is also a little thicker than water, likely due to complex polysaccharides similar to those contained in seaweed-derived thickening agents like agar agar and karageenan -- I was interested in keeping this property, although for some applications it might be unnoticeable or even undesirable.

I prepared some low-iodine dashi as described above -- either simmering kombu for 5 minutes or soaking for 30 minutes, then soaking in fresh water overnight. I found that the dashi from simmered kombu was very thin, and a little bitter, while the dashi from soaked kombu had more body and a better flavour. As expected they were both largely missing the distinctive MSG flavour, but after adding that back in I was largely happy with how the dashi tasted.

## Conclusions

TODO mention online recipes with loads of kombu
- might want to be careful about prepared products, iodine content often overlooked

## Prior work

- Japanese study
- other studies?

## Appendix: Experimental details

After some initial tests to check the reaction really works, I dialed in a reproducible procedure: one drop starch solution (from 3g cornstarch boiled with 97g water), one drop distilled vinegar, one drop of 3% hydrogen peroxide, and five drops of the sample liquid to be tested, mixed together in a white plastic well. Iodine in the sample causes the mixture to turn anywhere from pale pink to deep blue-black, with the colour change stabilising after about 10 minutes.

<!-- Next I moved on to some experiments to see how much iodine is removed from kombu after boiling for different amounts of time. I cut six pieces of kombu, approximately 3 grams each, and recorded their exact weights. Five of the pieces went into a litre of boiling water, and I removed one at 15 seconds, 50 seconds, 2 minutes, 5 minutes and 15 minutes, while the remaining piece was left untreated. I discarded that water, then boiled each of the six pieces individually in 300ml of water for 10 minutes to extract (most of) the remaining iodine. I further boiled each kombu extract down to a consistent volume relative to the intial mass of kombu (52ml for a 3g piece). To make the experiment more quantitative, I prepared three solutions of different concentrations of potassium iodide. -->

Next I moved on to some experiments to see how much iodine is removed from kombu after boiling for different amounts of time. I cut six small pieces of kombu. I boiled five of the samples for different lengths of time between 15 seconds and 15 minutes, and left the remaining sample untouched. I then boiled each of the six pieces in fresh water for 10 minutes to extract the remaining iodine, and reduced the resulting liquid down to concentrate it (ending up with 17 ml of liquid for each gram kombu). I also prepared three solutions of known concentrations of potassium iodide.

Kombu dashi has a slightly yellow colour, so to help identify the colour change from the reaction I ran a series of control experiments where I just added drops of water instead of vinegar and hydrogen peroxide.

<!-- calculate concentration: base solution was 200mg KI in 500ml water = 400mg KI per litre. I has atomic weight 126.9, KI has atomic weight 39.1+126.9. So concentration of I is 400*126.9/(126.9+39.1) = 306mg / L so molarity is 400e-3 / (39.1+126.9) = 2.4e-3. Other samples are diluted by a factor of 10 and a factor of 100.-->

![The top two rows show the iodine remaining in a piece of kombu after boiling for various lengths of time. The bottom row shows various known concentrations of iodine.](/images/kombu_experiment_1.png){.image-medium .image-pad-vertical .center alt="Results of the first experiment"}

![Control samples from the first experiment, showing the yellow colour of the kombu extract.](/images/kombu_experiment_1_control.jpg){.image-medium .image-pad-vertical .center alt="Control samples from the first experiment"}

There was no visible colour change for the 2 minute or longer samples, or for the 3 mg/L solution. The 50 second sample had a slight colour change, and the 15 second sample was a little lighter than the 30 mg/L solution. The untreated kombu had the most remaining iodine, turning a deep black and looking most similar to the 300 mg/L solution.

The results fit my expectations: the samples were about six times more concentrated than dashi prepared in the usual way, so suggest that dashi contains roughly 300/6 = 50 mg/L iodine, close to our previous guess of 30 mg/L. They also confirmed my hopes that even a very brief period of soaking or boiling could remove a lot of the iodine.

Most kombu dashi recipes either suggest cold soaking or heating at a bare simmer, never boiling hard. I wanted to try some practical recipes with soaking or simmering, and I also wanted to more accurately figure out how much iodine was left. This time I made two samples -- one was simmered for 5 minutes, and the other was soaked for 30 minutes. I then soaked each sample in fresh water overnight to extract the remaining iodine, before reducing the samples until only a few drops remained -- just 1.6ml liquid per gram of kombu. I prepared three known concentration solutions of potassium iodide, around the range I was expecting the samples to be in.

![The top row shows the iodine remaining in a piece of kombu after simmering for 5 minutes or cold soaking for 30 minutes. The bottom row shows various known concentrations of iodine.](/images/kombu_experiment_2.png){.image-medium .image-pad-vertical .center alt="Results of the second experiment"}

![Control samples from the second experiment.](/images/kombu_experiment_2_control.jpg){.image-medium .image-pad-vertical .center alt="Control samples from the second experiment"}

Even after reducing the sample so much, there was no detectable iodine in the sample simmered for five minutes, while the soaked sample appeared similar to the 30 mg/L solution. Taking into account how much we reduced the samples, this suggests that dashi made using pre-simmered kombu in this way would have less than 0.3 mg/L iodine, and dashi made with pre-soaked kombu would have about 0.5 mg/L, in both cases well within safe limits!

- Title: something about amateur chemistry?

- Safety disclaimer - peracetic acid. Medical disclaimer
- What is kombu?
- Mention some of my research e.g. paper in Japanese, Quora post claiming that traditionally people soaked kombu and discarded water, Australian Bonsoy recall
- Describe chemical reaction, calculate expected quantities
- Describe method, show results
- At end admit that process was much less neat then it looks?

Be sensitive to Japanese tradition - not definitely bad, just poorly understood. Context of goitrogens, unclear how bad lots of iodine is anyway. Make point that Asian populations might be adapted, like lactose tolerance?

Describe research first? Or make it more gripping by describing the experiments, then at the end explaining prior work.

Give full details? or not? or appendix?

Notes: kombu in dashi: https://justhungry.com/handbook/cooking-courses/japanese-cooking-101-lesson-1-its-all-about-dashi 10g per litre
https://www.justonecookbook.com/how-to-make-kombu-dashi-vegetarian-dashi/#wprm-recipe-container-59057 10g per litre
https://www.chopstickchronicles.com/kombu-dashi/#recipe 30g per litre


https://www.mja.com.au/journal/2010/193/7/iodine-toxicity-soy-milk-and-seaweed-ingestion-associated-serious-thyroid#0_i1095743
