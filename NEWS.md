# jaspModuleTemplate Changelog

> **HOW TO READ AND UPDATE THIS CHANGELOG:**
> 
> This document follows a modified [Keep a Changelog](https://keepachangelog.com/) format adapted for the R/JASP ecosystem. Releases are listed in reverse chronological order (newest first).
> As an example see [jaspModuleTemplate](https://github.com/jasp-stats/jaspModuleTemplate/blob/master/NEWS.md)
> * **Adding New Changes (For Contributors):** All new commits should be logged at the very top of the file under the `# jaspModuleTemplate (development version)` header. Place your bullet point under the appropriate category (`## Added`, `## Fixed`, etc.). 
> * **Issue References:** Please reference the relevant GitHub Issue (if any) at the end of your line (e.g., `([Issue #19](https://github.com/jasp-stats/jaspModuleTemplate/issues/19)`). 
> * **Format Categories:** >   * **Added:** New template features, QML examples, or build tools.
>   * **Changed:** Updates to default configurations, boilerplate code, or dependencies. 
>   * **Fixed:** Bug fixes in the build pipeline, R wrappers, or QML layouts.
>   * **Deprecated / Removed:** Outdated template components or legacy code.

---

# jaspBayesianQualityControl (development version)

## Added
* Bayesian process capability study: a standalone time series plot with the specification limits and target.
* Bayesian process capability study: a four-panel process overview (time series, prior and posterior distributions, sequential exceedance probability, and reference-prior sensitivity), with a selectable metric, threshold, reference prior, and sequential update binning.
* Bayesian process capability study: configurable process criteria with any number (at least two) of labelled regions, used by the interval table, the process overview, and the sequential analysis plots.

## Changed
* Bayesian process capability study: the Student-t model is now estimated with MCMC using the MCMC settings, and its custom priors (including the degrees of freedom) are taken from the Student-t prior list.

## Fixed
* Bayesian process capability study: an improper prior no longer prevents the posterior outputs; the prior-based plots explain why they are unavailable.

---

# jaspModuleTemplate (development version)

## Added
* Added NEWS.md
* Added workflow to remind users to update their `NEWS.md`.
* Added workflow to auto-bump version when user does not do so.

---

# jaspModuleTemplate 0.1.0

## Added
* Initial examples to showcase JASP module development

## Changed
* Use best practices for checking input ([Issue #19](https://github.com/jasp-stats/jaspModuleTemplate/issues/19)).
* The main results table now defaults to displaying 95% Confidence Intervals for effect sizes.

## Fixed
* Remove deprecated dependencies from qml files ([Issue #14](https://github.com/jasp-stats/jaspModuleTemplate/issues/14)).
