---
editor_options: 
  markdown: 
    wrap: 80
---

# HSE Behavioral Health Needs Score

## Data Pipeline

![](assets/Data%20Pipeline.png)

## Set-Up

Sync Microsoft Teams BHN Score Creation folder into your local The MITRE
Corporation (One-Drive) folder

## Finalize Measures

Measure Tracking document located in `Teams` \> `BHN Score` \>
`Measure-Tracking.xslx`

Refer to and update this document when additional measures are finalized.

## Pulling Data

Add data pulled from an API or directly downloaded from a website will fall into
one of the two folders:

-   `Teams` \> `BHN Score` \> `Data` \>`Raw`

    -   if any measure calculation needs to be completed (i.e. point geography
        to container geography, prevalence calculations, etc.)

    -   file extension is can also be .xlsx, .csv, .dta, etc

-   `Teams` \> `BHN Score` \> `Data` \>`Preprocessed`

    -   if data falls into a format where each row is a geographic container
        (i.e. Census Tract, County, ZIP Code, etc.)

    -   file extension is .csv only

    -   **Note:** For any data pulled from an API (tidycensus, etc.), perform
        any pre-processing tasks and write data directly to `Preprocessed`
        folder.

When pulling data, make sure to fill out relevant columns in the
`Measure-Tracking.xlsx` and / or `Metadata.xlsx` files.

## Measure Registration

Measure Registration document in `Teams` \> `BHN Score` \>`Data` \>
`Metadata.xslx` . This document provides information required for batch
processing / batch analysis from Pre-Processed data to Clean data.

### Pre-Processed Data

This file contains information on all **pre-processed** measures and informs any
additional transformations that need to occur between pre-processing and cleaned
data, including:

-   Any geographic level -\> ZCTA level only

-   Measure directionality aligned (higher values indicate higher need)

-   Scaling aligned (fractions to percents 0.1 -\> 10(%), prevalence adjustments
    (per 1000 people), etc.)

### Cleaned Data

Once data is cleaned, they will be merged into the Combined Measures file. There
will be two versions:

-   Original combined measures file: `Teams` \> `BHN Score` \>`Data` \> `Cleaned` \> `HSE_ZCTA_Converted_Measures.csv`

-   Percentile Scaled combined measures file: `Teams` \> `BHN Score` \>`Data` \> `Cleaned` \> `HSE_ZCTA_Percentile_Ranked_Measures.csv`

In addition, information about data (amount missing, number of nonmissing rows, etc.) is generated and appears in: `Teams` \> `BHN Score` \>`Data` \> `Cleaned` \> `HSE_BHN_Data_Information.csv`.

### Analysis

With Combined Measure files, we will perform the following analyses:

-   Covariance analysis (flagging and managing variable sets with high
    multi-collinearity)

-   Missingness analysis (identifying measures with high multiple missing
    values, identifying ZCTAs with multiple missing measures)

-   Cross-Validation with other composite measures (COI, UNS, CHR, SVI, etc.)

### Documentation

Measure documentation can be found in `Teams` \> `BHN Score` \>`Documentation`
folder.

## Weights

We will create 3 sets of weights files:

-   Parsimonious weighting (All equal weights)

-   Child Opportunity Index weighting determination method

-   County Health Rankings weighting

## Score Creation

Final scores for each ZCTA will be created by combining the weights and combined
measures file. Measures and measure weights are multiplied together, summed for
each ZCTA, and then re-scaled from 0 to 100. Scores appear (with percentile ranked measures) in `Teams` \> `BHN Score` \>`Data` \> `Cleaned`:

- Total Population Score: `HSE_BHN_ZCTA_Score_Black.csv`

- Black Population Score: `HSE_BHN_ZCTA_Score_Population.csv`

# 
