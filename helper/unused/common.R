#library(visdat) # sources plyr so needs to go early
library(purrr)
library(readxl)
library(stringr)
library(rlang)
library(magrittr)
library(ggthemes)
library(knitr)
library(DT)
library(assertthat)
library(lme4)
library(tidyverse)
library(eyetrackingR)
#library(langcog) # devtools::install_github("langcog/langcog")
# NOTE: Following install you may see errors, restarting R resolved for me
library(here)

opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE, cache = FALSE)
theme_set(theme_bw() + 
            theme(strip.background = element_blank(), 
                  panel.grid = element_blank())) # nice theme with limited extras
