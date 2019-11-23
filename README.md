LavaanMediators
====================================================

This is an R code repository for the analysis of a bi-factor model with two general factors, plus interaction and mediation effects, using the R package {lavaan}

Author(s)
-------

-   **Sunmee Kim** - <sunmee.kim@mail.mcgill.ca>

Updates & Notes
-------

1. Normality assumption in regular ML
  - There are ways to handle it. See pp.76-, Tutorial slides [here, Yves Rosseel 2014](https://personality-project.org/r/tutorials/summerschool.14/rosseel_sem_intro.pdf)
  - Bootstrapped standard errors of the parameter estimates: Bollen-Stine bootstrap
  - Examples? [here](https://psu-psychology.github.io/r-bootcamp-2018/talks/lavaan_tutorial.html) and [here](https://paolotoffanin.wordpress.com/2017/05/06/multiple-mediator-analysis-with-lavaan/comment-page-1/)

2. Interaction effects in {lavaan}
  - {semTools} package has a function indProd() that will generate product-indicators of a latent interaction.
  - See the example syntax for this regarding [indProd](https://www.rdocumentation.org/packages/semTools/versions/0.5-2/topics/indProd) and [probe2WayMC](https://www.rdocumentation.org/packages/semTools/versions/0.5-2/topics/probe2WayMC)
  - In {lavaan}, though, we can manually do this so-called residual centering approach of Little et al. (2006)
  - I found notes and replies from Holger Steinmetz on [this discussion](https://www.researchgate.net/post/Is_it_possible_to_conduct_moderated_mediation_with_latent_variables_with_available_packages_of_R_softwareAll_variables_are_continuous) useful.
  - Another reference: [Three Approaches to Estimate Latent Interaction Effects](file:///C:/Users/SUNMEE%20KIM/Downloads/Steinmetzetal.2011-Threeapproachestoestimatelatentinteractioneffects.pdf)
