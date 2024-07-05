# MB2P - Multiverse Approach to Data analysis - Preregistration

**Authors:**  
Giulia Calignano, Marlena Mayer, Robert Hepach

## Overview

The documents in this repository outlines the detailed methodology and analysis plan for the MB2P study investigating how children’s pupil dilation and looking time vary in response to goal and events. 
Using a multiverse approach, we explore different preprocessing paths and statistical models to ensure robustness and comprehensiveness in our findings beyond statistical significance.

## 1. Data Collection

Data have been collected as part of the Manybabies2 main study. The data collection was completed in 2023. The data is currently being curated by the main analysis team which will provide the raw data for the present study. The analyses conducted here will be based on the second test trial of the main study.

## 2. Main Questions and Hypotheses

### Main Question 1: Pupil Dilation and Goal-Incongruent Events

**Hypothesis 1.1:** Perceptual familiarity preference, pupil dilation will increase in response to the familiar outcome, i.e., the bear approaches the mouse’s box. This would be evident in a main effect of outcome.

**Hypothesis 1.2:** Perceptual novelty preference, pupil dilation will increase in response to the novel outcome, i.e., the bear approaches the empty box. This would be evident in a main effect of outcome.

**Hypothesis 1.3:** Conceptual familiarity preference, pupil dilation will increase in response to the bear approaching the mouse’s box in the knowledge condition only. This would be evident in an interaction effect of outcome and condition.

**Hypothesis 1.4:** Conceptual novelty preference results, pupil dilation will increase in response to the bear approaching the empty box in the knowledge condition only. This would be evident in an interaction effect of outcome and condition.

### Main Question 2: Cumulative Looking Time

We will test the same hypotheses as for Main Question 1 but for cumulative looking time instead of pupil dilation. Does observing goal-incongruent events result in longer cumulative looking time?

### Main Question 3: Surprise Response

**Hypothesis 3.1:** Knowledge / goal congruent condition. We expect a positive correlation between the differential AL score (proportion looking to correct AOI / [looking to correct + incorrect AOI]) and the VoE measures of change in pupil dilation and cumulative looking time. One possible pattern to indicate ‘surprise’ could be that the more children and adults anticipate the agent to act in line with their knowledge, the more they should be surprised to see that she does not act in line with their knowledge)

**Hypothesis 3.2:** Knowledge / goal incongruent condition. We expect a negative correlation between the differential AL-score. One possible pattern to indicate ‘surprise’ could be that the more children and adults anticipate the agent to act in line with their knowledge, the less they should be surprised to see that the bear does indeed act in line with their goal 

We will explore the relation between anticipation and pupil dilation (and cumulative looking time) but we do not have specific predictions with regards to the directionality of effects.

## 3. Key Dependent Variables

- **Baseline-Corrected Change in Pupil Dilation:** Measured during the second test trial, with specific preprocessing steps applied.
- **Cumulative Looking Time (including standard VoE Infant Controlled Measure):** Measured within a fixed time window of 30 seconds after the bear exits the tubes (details are provided in the online preregistration).
- **Degree of Anticipation:** We will use the same anticipatory looking score calculated as part of the main MB2 analysis (proportion looking to correct AOI/ looking to correct + incorrect AOI).


## 4. Conditions

Participants are randomly assigned to one of four conditions as part of the main MB2 study.

## 5. Analysis Plan

All code to implement and reproduce the pipeline explained here can be found in the .RMD and .HTML script in this repository.

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

- **Step 1:** Filter implausible pupil values (outside the range of 2-8mm in pupil size).
- **Step 2:** Generate datasets with plausible values only.

#### Second Degree of Freedom: Baseline Correction

#? One of these needs to include the 500ms before the bear resolution (i.e,. the bear exiting the tube) 
#? I thought we wanted to also include the baseline taken at the very beginning of the video, i.e., not time locked to the specific event?
#? I don't think this needs to include more than two methods unless we have a rationale for alternative baselines. Maybe Method 3 here would suffice given that it is also 500ms?

- **Method 1:** 5 seconds before the bear resolution.
- **Method 2:** 300 milliseconds after the bear resolution.
- **Method 3:** 500 milliseconds after the bear resolution.

#### Third Degree of Freedom: Participant Exclusion

#? This is what we had written in the prerigstration: "including only those subjects with two valid test trials, with one valid test trial (as defined by the main MB2-inclusion criteria), or with no trial exclusion criteria applied." This would be akin to three steps at this point, correct?

Participants are excluded based on predefined criteria for the first and second trials.

### Statistical Modeling

Generalized additive mixed modeling (GAMM) is used to analyze the pupillary data, incorporating random effects to handle hierarchical structures and smooth functions for both continuous and categorical predictors.

#### Models:

- **Model 1:** Linear model with condition*outcome*age.
- **Model 2:** Linear model with condition*outcome*age over time.
- **Model 3:** GAMM with condition*outcome*age, time, and participant-specific effects.

## 6. Secondary Analyses

In addition to the frequentist analyses, we will run Bayesian analyses for all hypotheses described above since Bayesian analyses allow us to capture evidence for as well as against an effect.

## 7. Sample Size

The sample size will be determined by the main MB2 study, and all available observations will be included.

## 8. Additional Explorations

We are going to explore the following additional questions:

-Does belief induction (as the mouse changes location in the test trial) increase pupil dilation?

-Does belief induction before the outcome relate to changes in pupil dilation after the outcome?

We will run supplementary analyses to investigate whether the overall luminance during testing sessions had an influence on pupil dilation changes and whether luminance effects could explain any of the fixed effects (see models above). We have collected data on how bright the testing environment was judged by participating labs (based on a questionnaire that was sent out) and the beginning of each testing session included a pupil calibration sequence which provides data on maximum and minimum pupil dilation for each participant.

Finally, we plan to visually explore gaze patterns to investigate whether children’s and adults’ gaze behavior differs across conditions.

## Of note
The multiverse approach increases the robustness of the present collaborative and pupillometry study [(Sirois et al., 2023)](10.1016/j.infbeh.2023.101890). 
By exploring multiple preprocessing paths and statistical models, we aim to provide robust results and informative interpretations in developmental pupillometry [(Calignano et al., 2023)](10.3758/s13428-023-02172-8).
