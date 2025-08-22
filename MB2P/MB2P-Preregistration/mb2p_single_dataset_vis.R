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
  
