# MB2P - Multiverse Approach to Data analysis - Preregistration

**Authors:**  
Giulia Calignano, Marlena Mayer, Robert Hepach

## Overview

The documents in this repository outlines the detailed methodology and analysis plan for the MB2P study investigating how children’s pupil dilation and looking time vary in response to goal and events. 
Using a multiverse approach, we explore different preprocessing paths and statistical models to ensure robustness and comprehensiveness in our findings beyond statistical significance.

## 1. Data Collection

Data for this study were collected as part of the Manybabies2 (MB2) main study, completed in 2023. 
The data is currently curated by the main analysis team, who will provide the raw data for our analyses.

## 2. Main Questions and Hypotheses

### Main Question 1: Pupil Dilation and Goal-Incongruent Events

**Hypothesis 1.1:** Perceptual familiarity preference leads to increased pupil dilation for familiar outcomes (bear approaches mouse’s box).

**Hypothesis 1.2:** Perceptual novelty preference leads to increased pupil dilation for novel outcomes (bear approaches empty box).

**Hypothesis 1.3:** Conceptual familiarity preference results in increased pupil dilation for familiar outcomes in the knowledge condition only.

**Hypothesis 1.4:** Conceptual novelty preference results in increased pupil dilation for novel outcomes in the knowledge condition only.

### Main Question 2: Cumulative Looking Time

We will test the same hypotheses as for Main Question 1 but for cumulative looking time instead of pupil dilation.

### Main Question 3: Surprise Response

**Hypothesis 3.1:** Positive correlation between anticipatory looking and changes in pupil dilation and looking time in the knowledge/goal congruent condition.

**Hypothesis 3.2:** Negative correlation between anticipatory looking and changes in pupil dilation and looking time in the knowledge/goal incongruent condition.

## 3. Key Dependent Variables

- **Baseline-Corrected Change in Pupil Dilation:** Measured during the second test trial, with specific preprocessing steps applied.
- **Total Looking Time:** Measured within a fixed time window of 30 seconds after the bear exits the tubes.
- **Standard VoE Infant Controlled Measure:** Looking time until the infant looks away for 2 seconds.
- **Degree of Anticipation:** Time taken for children to fixate on the correct area of interest (AOI).

## 4. Conditions

Participants are randomly assigned to one of four conditions as part of the main MB2 study.

## 5. Analysis Plan

All code to implement and reproduc the pipeline explained here can be found in the .RMD and .HTML script in this repository.

### Data Simulation

A simulated dataset, mirroring anticipated data characteristics, is created to pre-register preprocessing and analysis strategies. 

The simulated data includes:
- **participant_id:** Unique identifier for each participant.
- **age_cohort:** Categorizes participants into different age cohorts.
- **t:** Timestamp or duration.
- **x and y:** Coordinates or measurements.
- **pupil_left and pupil_right:** Measurements of pupil sizes.
- **lab_id:** Unique identifier for the lab.
- **conditions and outcomes:** Categorical variables representing the [MB2](https://manybabies.org/MB2/) manipulations.

### Multiverse Forking Paths

#### First Degree of Freedom: Filtering Extreme Pupil Values

- **Step 1:** Filter implausible pupil values (outside the range of 2-8mm).
- **Step 2:** Generate datasets with plausible values only.

#### Second Degree of Freedom: Baseline Correction

- **Method 1:** 5 seconds before the bear resolution.
- **Method 2:** 300 milliseconds after the bear resolution.
- **Method 3:** 500 milliseconds after the bear resolution.

#### Third Degree of Freedom: Participant Exclusion

Participants are excluded based on predefined criteria for the first and second trials.

### Statistical Modeling

Generalized additive mixed modeling (GAMM) is used to analyze the pupillary data, incorporating random effects to handle hierarchical structures and smooth functions for both continuous and categorical predictors.

#### Models:

- **Model 1:** Linear model with condition*outcome.
- **Model 2:** Linear model with condition*outcome over time.
- **Model 3:** GAMM with condition*outcome, time, and participant-specific effects.

## 6. Secondary Analyses

Bayesian analyses will complement frequentist analyses to capture evidence for and against effects.

## 7. Sample Size

The sample size will be determined by the main MB2 study, and all available observations will be included.

## 8. Additional Explorations

- Investigate the effect of belief induction on pupil dilation.
- Examine the relationship between belief induction before the outcome and changes in pupil dilation after the outcome.
- Conduct supplementary analyses to explore the influence of overall luminance during testing on pupil dilation changes.

## Of note
The multiverse approach increases the robustness of the present collaborative and pupillometry study [(Sirois et al., 2023)](10.1016/j.infbeh.2023.101890). 
By exploring multiple preprocessing paths and statistical models, we aim to provide robust results and informative interpretations in developmental pupillometry [(Calignano et al., 2023)](10.3758/s13428-023-02172-8).
