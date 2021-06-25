# To believe is not to think: A cross-cultural finding

Authors: Neil Van Leeuwen, Kara Weisman, & Tanya Marie Lurhmann

Are religious beliefs psychologically different from matter-of-fact beliefs? Many scholars say no: that religious people, in a matter-of-fact way, simply think their deities exist. Others say yes: that religious beliefs are more compartmentalized, less certain, and less responsive to evidence. Little research to date has explored whether lay people themselves recognize such a difference. We addressed this question in a series of sentence completion tasks, conducted in five settings that differed both in religious traditions and in language: the US, Ghana, Thailand, China, and Vanuatu. Participants everywhere routinely used different verbs to describe religious versus matter-of-fact beliefs, and they did so even when the ascribed belief contents were held fixed and only the surrounding context varied. These findings support the view that people from diverse cultures and language communities intuitively recognize a difference in attitude type between religious belief and everyday matter-of-fact belief.

This repo includes analyses of the quantitative data from these three studies, which took place in five international field sites as part of the [Mind and Spirit Project](https://themindandspiritproject.stanford.edu/#Home).

**Datasets** for each of the three studies are available [here](https://github.com/kgweisman/think_believe/tree/master/data).

**Analysis scripts** are organized by study. To view the results of an analysis in an HTML file, download the R Notebook (extension: .nb.html) to a folder on your computer and re-open it (from that folder) in a web browser -- or use the [htmlpreview.github.com](htmlpreview.github.com) links provided below. To view and manipulate the code, download the R Markdown file (extension: .Rmd) and open it in RStudio.

**Figures** are available [here](https://github.com/kgweisman/think_believe/tree/master/figures) -- HTML preview: [figures](http://htmlpreview.github.io/?https://github.com/kgweisman/think_believe/blob/master/figures/figures.nb.html)

## Study 1

Participants (N=344; n=48-97 per site) were presented with 25 sentences in one of two counterbalanced orders. Each sentence had the form “[Character] [thinks / believes] that X,” where X was a propositional phrase (e.g., John [thinks / believes] that Jesus Christ died for human sins). Participants selected one of the two words to complete the sentence. Ten of these sentences were “religious” (e.g., Jesus Christ died for human sins, the Buddha found spiritual truth while meditating). 
The remaining 15 sentences were “matter-of-fact” (e.g., Brazil is in South America, bronze contains more copper than tin, her dad is cooking noodles for dinner).

A mixed effects logistic regression revealed that, as predicted, participants were generally more likely to select “believe” (or its counterpart) to complete religious vs. matter-of-fact attitude ascriptions. This distinction was more pronounced in Thailand and the US, and less pronounced in Ghana; it did not differ from the grand mean in China or Vanuatu. Nonetheless, secondary analyses confirmed that this difference was significant in each field site considered alone.

![Figure 1](https://github.com/kgweisman/think_believe/blob/master/figures/journal_submission/fig_1.eps?raw=true)

HTML preview: [Study 1 analyses](http://htmlpreview.github.io/?https://github.com/kgweisman/think_believe/blob/master/think_believe_1.nb.html)

## Study 2

In Study 2, a separate group of participants (N=388; n=46-100 per site) completed the same sentences using a word or phrase of their own free choice. Methods were otherwise identical to Study 1.

Echoing Study 1, participants were more likely to generate responses containing the word-stem “believe” (or its counterpart) for religious compared to matter-of-fact attitude ascriptions. Indeed, in four of these five sites, “believe” (or its counterpart) appeared in the plurality of free responses to religious sentences. The difference between religious and matter-of-fact sentences was more pronounced in the US and China, less pronounced in Ghana and Vanuatu, and did not vary from the grand mean among participants in Thailand. But again, this difference was significant in each field site considered alone.

![Figure 2](https://github.com/kgweisman/think_believe/blob/master/figures/journal_submission/fig_2.eps?raw=true)

HTML preview: [Study 2 analyses](http://htmlpreview.github.io/?https://github.com/kgweisman/think_believe/blob/master/think_believe_2.nb.html)

## Study 3

Study 3 (N=328; n=49-80 per site) provided a final, closely controlled test of our primary claim. Methods were similar to Study 1, except that, rather than completing attitude ascriptions that were either religious or matter-of-fact in content, there were five pairs of brief vignettes that set up either a matter-of-fact or religious context, but ended with the same final sentence which the participant had to complete. This allowed us to test ascriptions of cognitive attitudes while holding the ascribed content constant. Vignettes were presented in one of two counterbalanced orders, with paired vignettes separated from each other. “Religious” vignettes included diverse religious traditions, and “matter-of-fact” vignettes were scientific, historical, or commonsensical in nature. 

As predicted, participants were generally more likely to select “believe” (or its counterpart) when the sentence was embedded in a religious vignette as opposed to the matching matter-of-fact vignette. This distinction was more pronounced in the US and Thailand, less pronounced in Ghana, and did not differ from the grand mean among participants in China or Vanuatu. The difference was significant in four of the five sites considered alone. The difference was not significant in our sample from rural Ghana, but it did appear to be present in a sample of Ghanaian undergraduates in a small follow-up study (not shown here; see Supplement).

![Figure 3](https://github.com/kgweisman/think_believe/blob/master/figures/journal_submission/fig_3.eps?raw=true)

HTML preview: [Study 3 analyses](http://htmlpreview.github.io/?https://github.com/kgweisman/think_believe/blob/master/think_believe_3.nb.html)


