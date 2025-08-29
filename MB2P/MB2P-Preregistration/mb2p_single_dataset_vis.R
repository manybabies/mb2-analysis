library(cowplot)

df <- data_df5$data[[72]]
#unify time bins
df <- df %>%
  mutate(t_norm_downsampled = floor(t_norm / 100) * 100)

sum_df_time <- df %>%
  filter(!is.na(average)) %>%
  group_by(age_cohort,condition, outcome, t_norm_downsampled) %>%
  summarize(
    N = n(),
    mean = mean(average, na.rm = TRUE),
    mean_z = mean(average_z,na.rm=TRUE),
    se_z = sd(average_z, na.rm = TRUE) / sqrt(N),
    ci_z = qt(0.975, df = N - 1) * se_z
  )

ggplot(sum_df_time,aes(t_norm_downsampled,mean_z,color=outcome,fill=outcome,linetype=age_cohort))+
  geom_hline(yintercept=0, linetype="dashed")+
  geom_line() +
  geom_ribbon(aes(ymin=mean_z-ci_z, ymax=mean_z+ci_z),alpha=0.2,color=NA) +
  facet_grid(condition ~ age_cohort) +
  labs(x = "Time (ms)", y = "Pupil size (z-scored,relative change)", color = "Outcome",fill="Outcome", linetype = "Age Cohort") +
  theme_cowplot()+
  xlim(0,5000)

participant_summary <- df %>%
  filter(!is.na(average)) %>%
  group_by(participant_lab_id,age_cohort,condition, outcome) %>%
  summarize(
    N = sum(!is.na(average)),
    participant_mean = mean(average, na.rm = TRUE),
    participant_mean_z = mean(average_z,na.rm=TRUE),
  )

sum_df_participant <- participant_summary %>%
  group_by(age_cohort,condition, outcome) %>%
  summarize(
    N = n(),
    mean = mean(participant_mean, na.rm = TRUE),
    mean_z = mean(participant_mean_z,na.rm=TRUE),
    se_z = sd(participant_mean_z, na.rm = TRUE) / sqrt(N),
    ci_z = qt(0.975, df = N - 1) * se_z
  )

ggplot(sum_df_participant, aes(outcome, mean_z,color=outcome))+
  geom_hline(yintercept=0, linetype="dashed")+
  #geom_violin(data=participant_summary,aes(y=participant_mean_z))+
  geom_point(size=2)+
  geom_errorbar(aes(ymin=mean_z-ci_z, ymax=mean_z+ci_z), width=0) +
  facet_grid(condition ~ age_cohort) +
  labs(x = "Condition", y = "Pupil size (z-scored,relative change)", color = "Outcome") +
  theme_cowplot()

ggplot(sum_df_participant, aes(outcome, mean_z,color=outcome))+
  geom_hline(yintercept=0, linetype="dashed")+
  geom_violin(data=participant_summary,aes(y=participant_mean_z),fill=NA)+
  geom_point(size=2)+
  geom_errorbar(aes(ymin=mean_z-ci_z, ymax=mean_z+ci_z), width=0) +
  facet_grid(condition ~ age_cohort) +
  labs(x = "Condition", y = "Pupil size (z-scored,relative change)", color = "Outcome") +
  theme_cowplot()

df.short <- df |>
  filter(t_norm >= 0 & t_norm < 5000) |>      
  group_by(participant_lab_id, condition_c, outcome_c, age_cohort,age_cohort_c, lab_id) |>
  summarize(Average = mean(average_z, na.rm=T)) |>
  ungroup() |> 
  mutate(
    age_cohort_adults = ifelse(age_cohort=="adults",0,1),
    age_cohort_toddlers =  ifelse(age_cohort=="toddlers",0,1),
  )

m <- lmer(Average ~ condition_c * outcome_c * age_cohort_c + (1|lab_id), data = df.short)  
summary(m)

m_toddlers <- lmer(Average ~ condition_c * outcome_c * age_cohort_toddlers + (1|lab_id), data = df.short)  
summary(m_toddlers)

m_adults <- lmer(Average ~ condition_c * outcome_c * age_cohort_adults + (1|lab_id), data = df.short)  
summary(m_adults)

#toddlers only
m_toddlers <- lmer(Average ~ condition_c * outcome_c + (1+condition_c+condition_c:outcome_c||lab_id), data = filter(df.short,age_cohort=="toddlers"))  
summary(m_toddlers)
allFit(m_toddlers)

#adults only
#toddlers only
m_adults <- lmer(Average ~ condition_c * outcome_c + (1+condition_c|lab_id), data = filter(df.short,age_cohort=="adults"))  
summary(m_adults)
