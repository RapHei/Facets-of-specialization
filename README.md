# Repository: **Facets of specialization.**

This repository contains code and data associated with the paper *Facets of Specialization and Its Relation to Career Success: An Analysis of U.S. Sociology, 1980-2015*. 
The paper can be found [here](https://journals.sagepub.com/doi/10.1177/00031224211056267).

If you use any of the code or ideas presented here, please cite our paper:

* Heiberger, Raphael H., Munoz-Najar Galvez, Sebastian, and McFarland, Dan A. (2021). Facets of Specialization and Its Relation to Career Success: An Analysis of U.S. Sociology, 1980 to 2015. *American Sociological Review*, 86(5).


## Summary
We investigate how sociology students garner recognition from niche field audiences through specialization. Our dataset comprises over 80,000 sociology-related dissertations completed at U.S. universities, as well as data on graduates’ pursuant publications. We analyze different facets of how students specialize – topic choice, focus, novelty, and consistency. To measure specialization types within a consistent methodological frame, we employ structural topic modeling. These measures capture specialization strategies employed at an early career stage. We connect them to a crucial long-term outcome in academia: becoming an advisor. Event-history models reveal that specific topic choices and novel combinations exhibit a positive influence, while focused theses make no substantial difference. In particular, theses related to the cultural turn, methods, or race are tied to academic careers that lead up to mentorship. Thematic consistency of students’ publication track has also a strong positive effect on their chances of becoming advisors. Yet, there are diminishing returns to consistency for highly productive scholars, adding an important nuance to the well-known imperative of publish or perish in academic careers.


## Data structure
The repository contains all data necessary to construct our 4 key metrics. 
For that purpose, we provide:

* Topic loads for each thesis: `Theta_Repo.RData` 
* Topic loads for students' publication: `Theta_WoS_Repo.RData`
* Students' characteristics: `Main_Repo.dta`
* Topic trends as estimated by `estimate.effects` from `stm`-package. Due to the file's size, please download it here: [Repo_stm.effect](https://bwsyncandshare.kit.edu/s/a3w7tznXLiAKZAB)

All those files rest on the main stm estimated with 60 topics:
`model <- stm(documents = out$documents, 
               vocab = out$vocab, 
               K = 60, 
               prevalence =~ s(Year),
               max.em.its = 75, 
               emtol = .000015 , 
               verbose = F, 
               data = out$meta,
               init.type = "Spectral")`

The raw text data can be found here:

* [ProQuest](https://www.proquest.com/)
* [Web of Science](https://www.proquest.com/)

Due to legal issues, we cannot provide the raw data with dissertation abstracts, advisees and advisor names, or departments in this repository. We kindly kind interested readers to the links above. However, the repository contains all data necessary to reproduce all results and shows how we construct our 4 key metrics. 

## Code
The repository hosts the R-code to construct each *facet of specialization* as discussed in the paper: 

* **Targeted Specialization**: `Repo_TargetSpec.R`
* **Focus**: `Repo_HHI.R`
* **Novelty**: `Repo_Novelty.R`
* **Consistency**: `Repo_Consistency.R` 

For *targeted specialization*, *focus*, and *novelty*, we used theses' topic loads (`Theta_Repo.RData`).
*Consistency* measures the closeness of a student's publications to their dissertation, so we use `Theta_Repo.RData` and `Theta_WoS_Repo.RData`.

Once all metrics are ready, we can merge them using `Repo_CreateMainDF.R` to derive the final event-history dataset.
 
The main results of the paper (Table 3) and Figures 5 and 6 are then calculated with `Stata`. The necessary code is included in this repo under "Code/Stata". Figure 4 uses output from `Stata` (coefficients, SE, pvalues), but is produced by `ggplot`.

Furthermore, please refer to `Repo_Fig2_Networks.R` and `Repo_Fig3_Trends.R`, respectively, to reconstruct Figures 2 and 3. For the latter, you need `Repo_stm.effect` as input (please see link above to retrieve data).

Finally, Figure 1 presents insights on the number of sociology students and their demographics. The data mirrors the figures of the ASA on [ethnicity](https://www.asanet.org/academic-professional-resources/data-about-disipline/data-dashboard/degrees-awarded/doctorates-awarded-sociology-race-or-ethnicity) and [gender](https://www.asanet.org/academic-professional-resources/data-about-disipline/data-dashboard/degrees-awarded/doctorates-awarded-sociology-gender), and [NSF Survey of Earned Doctorates](https://ncsesdata.nsf.gov/home/). Figure 1 was made using `Repo_Fig1_FieldStats.R`.

## Dependencies
To run the R scripts described above you will need to install a series of packages using the following command:
`install.packages(c("Desctools","dotwhisker","plyr","fastdummies","foreign","ggplot2", "ggrepel","grid","gtools","igraph","plyr","RColorBrewer","rgexf","reshape2","stm","stringr"))`.
