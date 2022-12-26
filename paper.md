---
title: 'dcurves: An R Package for Decision Curve Analysis'
tags:
  - R
  - decision-curve-analysis
  - model-evaluation
  - machine-learning
  - modeling
authors:
  - name: Daniel D. Sjoberg
    orcid: 0000-0003-0862-2018
    equal-contrib: false
    affiliation: 1 
  - name: Emily Vertosick
    equal-contrib: false
    affiliation: 1
affiliations:
 - name: Memorial Sloan Kettering Cancer Center, New York, New York, USA
   index: 1
date: 23 December 2022
bibliography: paper.bib
---

# Summary

Diagnostic and prognostic models are typically evaluated with measures of accuracy that do not address clinical consequences.
Decision-analytic techniques allow assessment of clinical outcomes but often require collection of additional information and may be cumbersome to apply to models that yield a continuous result.
Decision curve analysis is a method for evaluating and comparing prediction models that incorporates clinical consequences, requires only the data set on which the models are tested, and can be applied to models that have either continuous or dichotomous results.

While it is possible to compare the performance of different models using measures such as discrimination and calibration, these methods do not provide insight into whether the application of a model would be appropriate or beneficial in real-world circumstances.
For example, consider the question of whether a model that includes age, sex, and cholesterol is more effective at identifying patients with chest pain who are at risk of coronary artery disease (CAD) and should undergo cardiac catheterization, compared to a model that only includes age and sex.
In order to use the new model, it would be necessary to perform a blood draw to measure cholesterol.
However, if the new model can accurately identify patients with CAD while also allowing patients without CAD to avoid the invasive cardiac catheterization procedure, then the additional blood draw may be justified.
Decision curves can help us evaluate the trade-off between the risks and benefits of including cholesterol in the model.

# Statement of need

`dcurves` is an R package for calculating the decision curves associated with a model. 
Diagnostic and prognostic models are typically evaluated with measures of accuracy that do not address clinical consequences.
Decision-analytic techniques allow assessment of clinical outcomes but often require collection of additional information and may be cumbersome to apply to models that yield a continuous result.
Decision curve analysis is a method for evaluating and comparing prediction models that incorporates clinical consequences, requires only the data set on which the models are tested, and can be applied to models that have either continuous or dichotomous results.

The `dcurves` package includes methods for evaluating predictive models with binary `[@vickers:2006]` and time-to-event endpoints `[@vickers:2008]`.
The package also includes methods for model evaluation using decision curve analysis in the case-control setting `[@pfeiffer:2020]`.

# Decision Curve Analysis Usage

Before the `dcurves` package was made available on GitHub and CRAN, it was available as a downloadable script from the Memorial Sloan Kettering Cancer website.
The script had been downloaded and utilized thousands of times for manuscripts published in peer-reviewed journals with the primary decision curve analysis paper being cited nearly 3000 times.
Since the script was wrapped into the `dcurves` R package just over one year ago, it has been installed from CRAN more than 11,000 times.
The `dcurves` R package has an associated long-form documentation website at [www.danieldsjoberg.com/dcurves](https://www.danieldsjoberg.com/dcurves/), which includes examples of each function in the package and vignettes walking users through various use cases. 

Notably, decision curve analysis was used to assess the performance of a newly developed model for predicting the outcomes of prostate biopsies `[@parekh:2015]`.
This model is now commercially available and tens of thousands of clinicians in the USA and Europe order the test for their patients.

