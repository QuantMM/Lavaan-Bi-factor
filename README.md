Lavaan: Bi-factor
====================================================

This is an R code repository for the analysis of a bi-factor model with two general factors, plus interaction (moderation) and mediation effects, using the R package {lavaan}


Author(s)
-------

-   **Sunmee Kim** - <sunmee.kim@mail.mcgill.ca>


Updates & Notes
-------

**Summary report for the two points below: See "ResultsSummary_Jan242020.md"**

1. Normality assumption in regular ML
  - There are ways to handle it. See pp.76-, Tutorial slides [here, Yves Rosseel 2014](https://personality-project.org/r/tutorials/summerschool.14/rosseel_sem_intro.pdf)
  - Bootstrapped standard errors of the parameter estimates: Bollen-Stine bootstrap
  - Examples? [here](https://psu-psychology.github.io/r-bootcamp-2018/talks/lavaan_tutorial.html) and [here](https://paolotoffanin.wordpress.com/2017/05/06/multiple-mediator-analysis-with-lavaan/comment-page-1/)

2. Dealing with interaction effects (moderations) in {lavaan}
  - I found a comment from [this discussion](https://groups.google.com/forum/#!topic/lavaan/iP4LDqyjlLQ) helpful (by Terrence D. Jorgensen): create product indicators as indicators of a latent variable that represents the interaction term
  - {semTools} package has a function indProd() that will generate product-indicators of a latent interaction.
  - See the example syntax for this regarding [indProd](https://www.rdocumentation.org/packages/semTools/versions/0.5-2/topics/indProd) and [probe2WayMC](https://www.rdocumentation.org/packages/semTools/versions/0.5-2/topics/probe2WayMC)
  - In {lavaan}, though, we can manually do this so-called residual centering approach of Little et al. (2006)
  - I found notes and replies from Holger Steinmetz on [this discussion](https://www.researchgate.net/post/Is_it_possible_to_conduct_moderated_mediation_with_latent_variables_with_available_packages_of_R_softwareAll_variables_are_continuous) useful.
  - Another reference: [Three Approaches to Estimate Latent Interaction Effects](file:///C:/Users/SUNMEE%20KIM/Downloads/Steinmetzetal.2011-Threeapproachestoestimatelatentinteractioneffects.pdf)


**Final code

1. Baseline model: without the mediation effect of Negative Emotionality
  - See "Codes_July18_2020.r"
  - Some skipped code lines can be found in the code file below
  
2. Final analysis: to investigate the mediation effect of Negative Emotionality
  - See "Codes_May19_2020.r"

Notes on RMarkdown+Git
-------

1. (highly recommended): Install [Git for Windows](https://gitforwindows.org/), also known as msysgit or "Git Bash". This will ensure that the Git executable on my Windows system is found at 'C:/Program Files/Git/bin/git.exe'.

2. You can create an R Markdown document in RStudio, and commit&push the changes to GitHub.

3. Or, create a file with extension .Rmd on your RStudio using ```output: rmarkdown::github_document``` instead of ```output: html_document```, then simply upload the file on Github
