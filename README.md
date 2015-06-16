# Real-time Perceptions of Financial Market Stress

**Note: Work In Progress.**

## What

Indicator of real-time perceptions of financial market stress based on machine
classification of [Economist Intelligence Unit](http://www.eiu.com/) monthly
country reports.

Currently the Index is estimated for all EIU countries from 2003 through 2011.

## Motivation

Research into the causes, responses to, and effects of banking crisis needs a measure of banking crises that is: accurate, reliable, comparable across countries, and ideally includes information about crisis severity. Most work to-date uses one of two series of crisis data: Reinhart and Rogoff (2009) or Laeven and Valencia (2013) and its predecessors. These measures are lacking in that they are constructed post-hoc and so tend to be biased towards severe crises and away from circumstances where governments effectively calmed emerging trouble. This creates clear selection bias. In addition, they are simple dichotomous indicators of financial crisis and do so do not indicate crisis severity. We use a kernel principal component analysis (PCA) of Economist Intelligence Unit monthly country reports to develop a new real-time and continuous measure of perceived banking system stress. We refer to this measure as the EIU Perceptions of Financial Market Stress (EPFMS) Index. We not only develop a novel indicator of financial market stress, but also make a contribution to the wider political science and finance literatures on measurement by demonstrating how kernel PCA can be used to summarize vast quantities of qualitative texts into useful cross-sectional time-series indicators.

## Preview

The following figure shows a selection of financial market stress perceptions scores (solid grey lines). It also compares these scores to other measures of financial market stress and crisis. Dashed lines show Romer and Romer (2015). Red boxes are for Reinhart and Rogoff (2009). Yellow boxes are for Laeven and Valencia (2013).

![perceptions index plot](perceptions_compare.png)

## Download

To download the development version of the Index into R use:

```{S}
URL <- 'https://raw.githubusercontent.com/christophergandrud/EIUCrisesMeasure/master/data/results_kpca_rescaled.csv'
epfms_index <- rio::import(URL)
```

The primary indicator is labelled: `C1_ma`. See the
[paper](https://github.com/christophergandrud/EIUCrisesMeasure/blob/master/summary_paper/summary_paper.pdf)
describing the Index for more details.
