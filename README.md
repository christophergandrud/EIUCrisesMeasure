# FinStress: Real-time Perceptions of Financial Market Stress

**Note: Work In Progress.**

## What

Indicator of real-time perceptions of financial market stress based on machine
classification of [Economist Intelligence Unit](http://www.eiu.com/) monthly
country reports.

Currently the Index is estimated for all EIU countries from 2003 through 2011.

## About

Responding to financial market disruptions is one of the defining challenges for policymakers. Measures of financial conditions have significant shortcomings. Binary crisis indicators (e.g. Laeven and Valencia 2013) are constructed post hoc, biasing them towards severe events. This has important theoretical implications--studies using these variables cannot differentiate between periods of calm and periods where governments effectively addressed emerging trouble. Continuous indicators capture quantities whose importance, measurement, and reporting varies significantly across countries. We use a kernel principal component analysis (PCA) of Economist monthly country reports to create a continuous measure of real-time perceived stress severity. Not only does it more accurately capture a key concept political scientists often want to study, but it can be used to test theories that were previously difficult to test. We also demonstrate how kernel PCA can be used to summarize large corpora of texts into continuous indicators.

## Preview

The following figure shows a selection of financial market stress perceptions scores (solid grey lines). It also compares these scores to other measures of financial market stress and crisis. Dashed lines show Romer and Romer (2015). Red boxes are for Reinhart and Rogoff (2009). Yellow boxes are for Laeven and Valencia (2013).

![perceptions index plot](perceptions_compare.png)

## Download

To download the development version of the Index into R use:

```{S}
URL <- 'https://raw.githubusercontent.com/christophergandrud/EIUCrisesMeasure/master/data/FinStress.csv'
finstress_index <- rio::import(URL)
```

The primary indicator is labelled: `FinStress`. See the
[paper](https://github.com/christophergandrud/EIUCrisesMeasure/blob/master/summary_paper/summary_paper.pdf)
describing the Index for more details.
