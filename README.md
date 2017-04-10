# FinStress: Real-time Perceptions of Financial Market Stress

*British Journal of Political Science*

Christopher Gandrud and Mark Hallerberg

## What

Indicator of real-time perceptions of financial market stress based on machine
classification of [Economist Intelligence Unit](http://www.eiu.com/) monthly
country reports.

Currently the Index is estimated for all EIU countries from 2003 through 2011.

## About

Responding to financial market disruptions is a defining challenge for policymakers and a central topic of political research. Yet established measures of financial conditions have significant shortcomings. Annual binary crisis variables limit our ability to explore non-linear relationships and the political effects of rapidly changing conditions. Continuous indicators have their own flaws in operationalization and reproducability. We create a continuous measure of real-time perceived stress using a kernel principal component analysis (KPCA) of *Economist* monthly country reports. We demonstrate the usefulness of our measure by showing that it more accurately captures the effect of financial market stress levels on electoral volatility. We also show how KPCA can be used to efficiently summarize large quantities of texts into cross-sectional time-series variables.

## Preview

The following figure shows a selection of financial market stress perceptions scores (solid grey lines). It also compares these scores to other measures of financial market stress and crisis. Dashed lines show Romer and Romer (2015). Red boxes are for Reinhart and Rogoff (2009). Yellow boxes are for Laeven and Valencia (2013).

![perceptions index plot](perceptions_compare.png)

## Download

To download the development version of the Index into R use:

```{S id:"j1bvfiw5"}
URL <- 'https://raw.githubusercontent.com/christophergandrud/EIUCrisesMeasure/master/data/FinStress.csv'
finstress_index <- rio::import(URL)
```

The primary indicator is labelled: `FinStress`. See the
[paper](https://github.com/christophergandrud/EIUCrisesMeasure/blob/master/summary_paper/summary_paper.pdf)
describing the Index for more details.


## Reproduction

This directory contains the following reproduction material:

-   *summary_paper/analysis*: source code for further analysis of the FinStress
Index including using it as a right-hand side variable in
explorations of political phenomenon as well as comparing it to
other measures of crisis and financial market stress.

-   *source*: replication material for creating the FinStress Index
of real-time perceptions of financial market stress. The directory
also contains alternative specifications and robustness analyses
discussed in the Online Appendix.

-   *data*: replication data, including the full FinStress Index
(*FinStress.csv*).

### Notes

-   The raw *Economist Intelligence Unit* texts are not included with this
reproduction material due to copyright restrictions.

-   You will likely need to change the working directories and introduce
appropriate files to output figures and tables in order to reproduce the
contents of these files.
