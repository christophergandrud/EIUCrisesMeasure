# Real-time Perceptions of Financial Market Stress

**Note: This work is in the early stages of development. It will be updated significantly.**

## What

Measure of real-time perceptions of financial market stress based on machine
classification of [Economist Intelligence Unit](http://www.eiu.com/) monthly
country reports.

## Motivation

Political economy research on the causes, responses to, and effects of banking crisis needs an accurate and reliable measure of banking crises that is comparable across countries and ideally includes information on crisis severity. Most research to date uses one of two series of crisis data: Reinhart and Rogoff (2009) or Laeven and Valencia (2013). These measures are lacking in that they are simple dichotomous indicators of financial crisis and differ considerably in their start and end dates for many incidents. They are also constructed after the fact and so tend to be biased towards severe crises and away from those where government responses effectively calmed emerging trouble. To overcome these issues, we use a kernel principal component analysis (PCA) of Economist Intelligence Unit monthly country reports to develop a new real-time and continuous measure of banking system stress. We refer to this measure as the EIU Perceptions of Financial Market Stress (EPFMS) Index. In doing so, we not only develop a novel indicator of financial market stress, but also make a contribution to the wider political science literature on measurement by demonstrating how kernel PCA can be used to summarize vast quantities of qualitative texts into useful cross-sectional time-series indicators.

## Preview

The following figure shows a selection of financial market stress perceptions scores (solid grey lines). It also compares these scores to other measures of financial market stress and crisis. Dashed lines show Romer and Romer (2015). Red boxes are for Reinhart and Rogoff (2009). Yellow boxes are for Laeven and Valencia (2013).

![perceptions index plot](perceptions_compare.png)
