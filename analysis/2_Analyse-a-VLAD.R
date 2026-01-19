#### LOAD LIBRARIES ####
library(arrow)
library(lme4)
library(ggplot2)
library(tidyr)
library(dplyr)
library(glmmTMB)
library(performance)
library(parameters)
library(modelbased)
library(emmeans)
library(DHARMa)

#### Helper functions ####
plot_effect_with_ci <- function(
    data, 
    x_var = "estimate", 
    y_var = "contrast", 
    lower = "asymp.LCL", 
    upper = "asymp.UCL", 
    threshold = 0, 
    p.value = FALSE,
    title = "",
    point_size = 1,
    errorbar_width = 0.2,
    errorbar_size = 0.5,
    thresh_color = "red",
    thresh_type = "longdash",
    thresh_size = 0.5,
    thresh_alpha = 1,
    theme = theme_bw(),
    transpose = FALSE
) {
  
  # Base plot
  if (transpose) {
    p <- ggplot(data, aes_string(x = y_var, y = x_var)) +
      geom_errorbar(
        aes_string(ymin = lower, ymax = upper),
        width = errorbar_width,
        size = errorbar_size
      )
  } else {
    p <- ggplot(data, aes_string(y = y_var, x = x_var)) +
      geom_errorbar(
        aes_string(xmin = lower, xmax = upper),
        width = errorbar_width,
        size = errorbar_size
      )
  }
  p <- p +
    geom_point(size = point_size)
  if (transpose) {
    p <- p +
      geom_hline(
        yintercept = threshold,
        linetype = thresh_type,
        color = thresh_color,
        linewidth = thresh_size,
        alpha = thresh_alpha
      )
  } else {
    p <- p +
      geom_vline(
        xintercept = threshold,
        linetype = thresh_type,
        color = thresh_color,
        linewidth = thresh_size,
        alpha = thresh_alpha
      ) +
      scale_y_discrete(limits=rev)
  }
  p <- p +
    ggtitle(title) +
    theme_bw() +
    theme(plot.title = element_text(vjust = 0.5, lineheight = 1.2))
  
  # Optionally add p-value labels
  if (p.value && "p.value" %in% colnames(data)) {
    p <- p + geom_text(
      aes_string(label = "scales::pvalue(p.value, accuracy = 0.01)"),
      vjust = -0.5,
      size = 3
    )
  }
  
  return(p)
}

working_directory <- getwd()
# working_directory <- "/home/davor/Documents/vladislav"

#### LOAD DATA ####
df_full <- read_parquet(paste0(working_directory, "/vladislav_mph_trials.parquet"))
df_full <- df_full %>% filter(reliability == 1)
df_full <- df_full %>% rename(treatment = group)
df_full$treatment <- as.factor(df_full$treatment)
df_full$session_part <- as.factor(df_full$session_part)

# We will also import the dataframe where the mean of each day was taken to use as a reference point.
df_full_binned <- read_parquet(paste0(working_directory, "/vladislav_mph_trials_daybin.parquet"))
df_full_binned <- df_full_binned %>% rename(treatment = group)

##### Summary heatmap ----------------------------------------------------------
# First, let's create a nice visualisation of when each animal reached a threshold of 80% correct responses,
# based on their daily means.
# We need to load the day-binned dataset.
hm_protocol <- "test"
hm_difficulty <- "easy"
heatmap_data <- df_full_binned %>%  
  filter((protocol == hm_protocol) & (difficulty == hm_difficulty) & (session_part == "overall"))
if (hm_protocol == "test") {
  heatmap_data$correct_pct <- heatmap_data$correct/heatmap_data$precue_correct
} else {
  heatmap_data$correct_pct <- heatmap_data$correct/(heatmap_data$extension_happened+heatmap_data$correct)
  # heatmap_data$correct_pct <- heatmap_data$correct/(heatmap_data$iti_extension+heatmap_data$correct)
}
correct_threshold <- 0.66
learned_sign <- "+"

if (hm_protocol == "test"){
  heatmap_data <- heatmap_data %>%  
    select(animal, treatment, day, correct, precue_correct, correct_pct)  
} else {
  heatmap_data <- heatmap_data %>%  
    select(animal, treatment, day, correct, extension_happened, correct_pct)
}


# To be able to add a + at the end of a heatmap row for animals which failed to meet
# the threshold, we need to add an additional day after the last one which will have
# correct_pct = 1 for all animals.
threshold_indication <- heatmap_data %>%  filter(day == max(day))
threshold_indication$correct_pct <- 1
threshold_indication$day <- threshold_indication$day+1
# To plot the data, we will use the `fill` column, which will be a copy of `correct_pct`,
# except for the indicator data, which will be NA. This will enable us to use `na.value`
# to fix this column's tile colour to white.
heatmap_data$fill <- heatmap_data$correct_pct
threshold_indication$fill <- NA

data <- rbind(heatmap_data, threshold_indication)
data <- data %>% 
  group_by(animal) %>%  
  mutate(learned = if_else(correct_pct > correct_threshold, learned_sign, "")) %>% 
  mutate(learned = if_else(is.nan(correct_pct), "/", learned)) %>% 
  mutate(learned = if_else(learned == learned_sign & cumsum(learned == learned_sign) > 1, "", learned)) %>% 
  mutate(border = if_else(learned == learned_sign & cumsum(learned == learned_sign), learned_sign, NA_character_))

border_data <- data %>% 
  mutate(xmin = as.numeric(day) - 1, 
         xmax = as.numeric(day) + 1, 
         ymin = as.numeric(factor(animal)) - 1, 
         ymax = as.numeric(factor(animal)) + 1) %>%
  filter(!is.na(border))

ggplot(data, aes(day, animal, fill = fill)) +
  geom_tile(stat = "identity", color = "white", linewidth=1) + 
  #geom_tile(data = border_data, color = "black", fill = NA, linewidth=1) + 
  #facet_grid(~ donor, scales = "free_y", space = "free_y") +
  #scale_fill_viridis_c(option = "magma", direction = 1, begin = 0.3, labels = label_percent()) +
  scico::scale_fill_scico(palette = "turku", limits = c(0, 1), begin = 0, end = 1, direction = 1, alpha=0.8, labels = scales::label_percent(), na.value = "white")+
  theme_minimal() +
  theme(
    axis.text.y = element_text(vjust = 0.5, hjust = 1),
    axis.title.y = element_blank(),
    #strip.background = element_blank(),
    # panel.spacing = unit(1, "lines"),
    strip.text = element_text(size = 10, face = "bold", hjust = 0.5, vjust = 1.5),
    strip.placement = "outside",
    #strip.background = element_rect(color = "white", linewidth = 0)
  ) +
  xlab("") +
  ylab("animal") +
  guides(fill = guide_colorbar(barwidth = 1, barheight = 5)) +
  geom_text(aes(label = learned),
            color = "black",
            size = 4) +
  labs(fill = "Correct") +
  scale_x_discrete(expand = expansion(mult = c(0.01, 0.01))) +
  scale_y_discrete(expand = expansion(mult = c(0.01, 0.01))) +
  #facet_grid(factor(treatment)~., scales="free_y", switch="y")
  facet_wrap(~treatment, nrow=2, scales="free_y", strip.position = "left")
#ggsave(paste0(working_directory, "/1_criterion_heatmap.png"), width=18, height = 5.3, units = "cm")
ggsave(paste0(working_directory, "/1_criterion_heatmap.png"), width=16, height = 14, units = "cm")
#theme()
##
#############

df_training <- df_full %>% 
  filter(protocol == "training") #%>% 
  # filter(difficulty == "difficult")
df_training <- df_training %>% filter(!((correct == 0) & (iti_extension == 0)))
df_training$hour <- as.numeric(df_training$t_start-min(df_training$t_start))/3600
df_training$day <- df_training$day-min(df_training$day)

df_binned_training <- df_full_binned %>%
  filter(protocol == "training") #%>% 
  # filter(difficulty == "difficult")
df_binned_training$correct_pct <- df_binned_training$correct/(df_binned_training$correct+df_binned_training$iti_extension)
df_binned_training$correct_pct2 <- df_binned_training$correct/(df_binned_training$correct+df_binned_training$extension_happened)
df_binned_training$hour <- as.numeric(df_binned_training$t_start-min(df_binned_training$t_start))/3600
df_binned_training$day <- df_binned_training$day-min(df_binned_training$day)


ggplot(df_binned_training,
       aes(x=day, y=correct_pct, color=treatment)) +
  stat_summary_bin(geom="point", fun=mean, breaks = seq(min(df_training$day)-0.5, max(df_training$day)+0.5)) +
  stat_summary_bin(geom="errorbar", aes(width = after_stat(0)), fun.data=mean_se, breaks = seq(min(df_training$day)-0.5, max(df_training$day)+0.5)) +
  theme_bw(base_size=10)+
  theme(panel.grid.minor = element_blank())+
  scale_x_continuous(breaks=seq(min(df_training$day), max(df_training$day)))+
  scale_y_continuous(labels = scales::percent, limits = c(0,1))+
  ggtitle("Each day as a mean of every animal's daily mean")
ggplot(df_training,
       aes(x=day, y=correct, color=treatment)) +
  stat_summary_bin(geom="point", fun=mean, breaks = seq(min(df_training$day)-0.5, max(df_training$day)+0.5)) +
  stat_summary_bin(geom="errorbar", aes(width = after_stat(0)), fun.data=mean_se, breaks = seq(min(df_training$day)-0.5, max(df_training$day)+0.5)) +
  theme_bw(base_size=10)+
  theme(panel.grid.minor = element_blank())+
  scale_x_continuous(breaks=seq(min(df_training$day), max(df_training$day)))+
  scale_y_continuous(labels = scales::percent, limits = c(0,1))+
  ggtitle("Each day as a mean of all animals' trials")

# df_training$extension_happened <- as.numeric(as.logical(df_training$iti_extension))

df_training2 <- df_training %>% filter((correct == 1) | (extension_happened == 1))

selected_model <- glmmTMB(cbind(correct, iti_extension) ~
                            poly(hour,5)*as.factor(treatment) +
                            (poly(hour,2) | animal),
                          data = df_binned_training,
                          family = binomial(),
                          control = glmmTMBControl(parallel = parallel::detectCores()))

simulateResiduals(fittedModel = selected_model, plot = T, n = 1000)

### Plotting models fitted on day-binned data (df_binned_training)
ggplot(data = df_binned_training, aes(x = day, color = treatment)) +
  stat_summary(data = df_binned_training, aes(x = day, y = correct_pct, color = treatment), fun=mean, geom="point") +
  stat_summary(data = df_binned_training, aes(x = day, y = correct_pct, color = treatment, width=after_stat(0.2)), fun.data=mean_se, geom="errorbar") +
  stat_summary(aes(y=fitted(a1), color = treatment),
               #        breaks = c(0, seq(12,240,24)),
               fun=mean, geom="line") +
  theme_bw(base_size=10) +
  facet_wrap(~animal)+
  ylim(0,1)
### Plotting models fitted on full data (df_training)
# ggplot(data = df_training, aes(x = hour, color = treatment)) +
#   stat_summary_bin(data = df_binned_training,
#                    aes(x = hour, y = correct_pct2, color = treatment),
#                    fun=mean,
#                    geom="point",
#                    breaks = c(0, seq(12,240,24))) +
#   stat_summary_bin(data = df_binned_training,
#                    aes(x = hour, y = correct_pct2, color = treatment, width=after_stat(5)),
#                    fun.data=mean_se,
#                    geom="errorbar",
#                    breaks = c(0, seq(12,240,24))) +
#   stat_summary_bin(aes(y=fitted(a2), color = treatment),
#                    breaks = c(0, seq(12,240,24)),
#                    fun=mean, geom="line") +
#   theme_bw(base_size=10) +
#   facet_wrap(~animal)+
#   ylim(0,1)

model.contrasts.raw <- emmeans(selected_model, pairwise~treatment|hour, at=list(hour = unique(df_training$day)*24+2))
model.contrasts <- (model.contrasts.raw %>% summary(by = NULL, infer=c(TRUE,TRUE), adjust="sidak"))$contrast
# model.means <- estimate_means(selected_model, by=c("treatment", "hour"))
model.means <- (
  emmeans(
    selected_model,
    pairwise~treatment|hour,
    at = list(hour = seq(min(df_training$hour),
                         max(df_training$hour),
                         0.5)
    ),
    # at = list(hour = unique(df_training$day)/24),
    type = "response",
    adjust = "sidak"
  ) %>% 
    summary(by = NULL)
)$emmean
ggplot(model.contrasts, aes(estimate, y = hour, label = scales::pvalue(p.value, accuracy = 0.001)))+
  geom_point(size = 2)+
  geom_errorbar(aes(x = estimate,
                    xmin = asymp.LCL,
                    xmax = asymp.UCL),
                width = 0.3,
                size = 1)+
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red",
             size = 1,
             alpha = 1)+
  theme_bw()+
  ggtitle("Log odds ratio for correct response\n[CTR / STZ]")+
  theme(plot.title = element_text(vjust = 0.5, lineheight = 1.2))+
  ylab("")+
  xlab("")+
  geom_text(nudge_y = 10, size = 3.5)+
  scale_y_continuous(
    breaks = unique(df_binned_training$hour)+2,
    labels = function(x) { d = ((x-2) / 24)+1; paste("day", d) }
  )


ggplot(data = model.means, aes(x = hour, y = prob)) +
  # geom_violin() +
  # geom_jitter(width = 0.05) +
  # geom_ribbon(color=NA, alpha = 0.1,
  #             aes(ymin = asymp.LCL, ymax = asymp.UCL, fill = treatment)) +
  geom_line(aes(color = treatment), alpha=0.2) +
  geom_pointrange(
    data = model.means %>% filter(hour %in% c(unique(df_training$day)*24)),
    aes(y = prob, ymin = asymp.LCL, ymax = asymp.UCL, color = treatment),
    size = 1
    #color = treatment
  ) +
  # stat_summary_bin(data = df_test,
  #                  aes(
  #                    y = correct,
  #                    color = treatment,
  #                    width = after_stat(5)
  #                  ),
  #                  breaks = seq(min(df_binned_test$hour)-12,
  #                               max(df_binned_test$hour)+12,
  #                               by = 24),
  #                  #position = position_dodge(width = 0.1),
  #                  fun.data=mean_se, geom="errorbar") +
  theme_bw() +
  scale_x_continuous(
    breaks = unique(df_binned_training$hour),
    labels = function(x) { d = (x/24)+1 }
  ) +
  scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
  ylab("Probability of correct response") +
  xlab("Day") +
  # ggtitle(deparse1(model.to_plot$modelInfo$allForm$formula))
  ggtitle("Model-estimated learning curve")

# Last day
emmeans(selected_model, pairwise~treatment, at=list(hour = c(218)))
# Peak days - 6, 7, 8, 9
emmeans(selected_model, pairwise~treatment, at=list(hour = c(122, 146, 170, 194, 218)))
# Overall
emmeans(selected_model, pairwise~treatment)

###################
df_test <- df_full %>%
  filter(protocol == "test") %>% 
  filter(difficulty == "easy")
df_test <- df_test %>% filter(!is.na(correct))
df_test$hour <- as.numeric(df_test$t_start-min(df_test$t_start))/3600
df_test$day <- df_test$day-min(df_test$day)

df_binned_test <- df_full_binned %>% 
  filter(protocol == "test") %>% 
  filter(difficulty == "easy")
df_binned_test$correct_pct <- df_binned_test$correct/df_binned_test$precue_correct
df_binned_test$hour <- as.numeric(df_binned_test$t_start-min(df_binned_test$t_start))/3600
df_binned_test$day <- df_binned_test$day-min(df_binned_test$day)

# df_test$was_impulsive <- as.logical(df_test$impulsive_nosepokes)

#### CORRECT_PCT - TEST EASY ####
df_test <- df_full %>%
  filter(protocol == "test") %>% 
  filter(difficulty == "easy")
df_test <- df_test %>% filter(!is.na(correct))
df_test$hour <- as.numeric(df_test$t_start-min(df_test$t_start))/3600
df_test$day <- df_test$day-min(df_test$day)

df_binned_test <- df_full_binned %>% 
  filter(protocol == "test") %>% 
  filter(difficulty == "easy")
df_binned_test$correct_pct <- df_binned_test$correct/df_binned_test$precue_correct
df_binned_test$hour <- as.numeric(df_binned_test$t_start-min(df_binned_test$t_start))/3600
df_binned_test$day <- df_binned_test$day-min(df_binned_test$day)

ggplot(df_binned_test,
             aes(x=day, y=correct_pct, color=treatment)) +
  stat_summary_bin(geom="point", fun=mean, breaks = seq(min(df_test$day)-0.5, max(df_test$day)+0.5)) +
  stat_summary_bin(geom="errorbar", aes(width = after_stat(0)), fun.data=mean_se, breaks = seq(min(df_test$day)-0.5, max(df_test$day)+0.5)) +
  theme_bw(base_size=10)+
  theme(panel.grid.minor = element_blank())+
  scale_x_continuous(breaks=seq(min(df_test$day), max(df_test$day)))+
  ggtitle("Each day as a mean of every animal's daily mean")
ggplot(df_test,
       aes(x=day, y=precue_correct, color=treatment)) +
  stat_summary_bin(geom="point", fun=sum, breaks = seq(min(df_test$day)-0.5, max(df_test$day)+0.5)) +
  stat_summary_bin(geom="errorbar", aes(width = after_stat(0)), fun.data=mean_se, breaks = seq(min(df_test$day)-0.5, max(df_test$day)+0.5)) +
  theme_bw(base_size=10)+
  theme(panel.grid.minor = element_blank())+
  scale_x_continuous(breaks=seq(min(df_test$day), max(df_test$day)))+
  ggtitle("Each day as a mean of all animals' trials")+
  facet_wrap(~session_part)


# Both the grand mean and mean of means show the highest performance on day 6, after which it drops off,
# likely due to something happening outside the cage - perhaps behaviour,  though unlikely (large drop in
# performance on the day of open field (day 7), better performance on days of NOR (days 7 and 8)).
# As we'll be analysing the learning rate using curve-fitting, days after day 6 will only introduce noise
# the learning process, which we're interested in, is practically finished, i.e. the drop in performance
# after day 6 isn't due to (a failure of) learning, but curve-fitting would try to account for this data
# as well. Hence, we will limit curve-fitting to days 0-6.
# NOTE: In the plot, we'll start counting since day 1, while in this explanation, count starts from 0.
# df_test <- df_test %>% filter(day %in% 0:6)
# df_binned_test <- df_binned_test %>% filter(day %in% 0:6)

# vizdata <- estimate_relation(m.2t)
# ggplot(vizdata, aes(x = hour, y = Predicted, color=treatment)) +
#   geom_ribbon(aes(ymin = CI_low, ymax = CI_high), alpha = 0.1) +
#   geom_line() +
#   # Add original data points
#   geom_point(data = df_binned_test, aes(x = hour, y = correct_pct)) +
#   # Aesthetics
#   theme_minimal()


selected_model <- glmmTMB(correct ~
                            poly(hour,2)*as.factor(treatment)*as.factor(session_part) +
                            (0+hour | animal)+
                            (1 | session_part:day:animal),
                          data = df_test,
                          family = binomial(link = "logit"),
                          control = glmmTMBControl(parallel = parallel::detectCores()))
model_performance(selected_model)
# glmmTMB(correct ~
#           poly(hour,2)*as.factor(treatment)*as.factor(session_part) +
#           (hour | animal)+
#           (0+hour | session_part:day:animal),
#         data = df_test,
#         family = binomial(link = "logit"))

#compare_performance(selected_model, selected_model.orig, selected_model.prev)#, selected_model.orig2, selected_model.prev2)
check_overdispersion(selected_model)
# check_zeroinflation(selected_model)
check_collinearity(selected_model)
check_autocorrelation(selected_model)
sim.res <- simulateResiduals(selected_model, n = 1000, plot = F)
plot(sim.res)

ggplot(data = df_test, aes(x = hour,
                           color = treatment
                           )) +
  stat_summary_bin(data = df_test,
                   aes(x = hour, y = correct),#, color = bhv_performed),
                   fun=mean,
                   geom="point",
                   breaks = c(0, seq(12,240,24))) +
  stat_summary_bin(data = df_test,
                   aes(x = hour, y = correct),# color = bhv_performed),
                   fun.data=mean_se,
                   geom="errorbar",
                   breaks = c(0, seq(12,240,24))) +
  stat_summary_bin(aes(y=fitted(selected_model)),#, color = bhv_performed),
                   breaks = c(0, seq(12,240,24)),
                   fun=mean, geom="line") +
  theme_bw(base_size=10) +
  #scale_y_sqrt()+
  facet_wrap(~animal*session_part)
ggplot(data = df_test, aes(x = day,
                           color = treatment
)) +
  stat_summary_bin(data = df_test,
                   aes(x = day, y = correct),#, color = bhv_performed),
                   fun=mean,
                   geom="point",
                   #breaks = c(0, seq(12,240,24))
                   ) +
  stat_summary_bin(data = df_test,
                   aes(x = day, y = correct),# color = bhv_performed),
                   fun.data=mean_se,
                   geom="errorbar",
                   #breaks = c(0, seq(12,240,24))
                   ) +
  stat_summary_bin(aes(y=fitted(selected_model)),#, color = bhv_performed),
                   #breaks = c(0, seq(12,240,24)),
                   fun=mean, geom="line") +
  theme_bw(base_size=10) +
  #scale_y_sqrt()+
  facet_wrap(~animal*session_part)

model.means.raw <- emmeans(
  selected_model,
  ~treatment|hour,
  at = list(hour = unique(df_test$day)*24+2)
)
model.means <- summary(model.means.raw, by = NULL, type="response")
model.contrasts.raw <- contrast(model.means.raw,
                                method = "revpairwise",
                                #at=list(hour = unique(df_test$day)*24+2),
                                infer=c(T,T),
                                type = "response"
                                )
model.contrasts <- model.contrasts.raw %>% summary(by = NULL)
# model.means <- estimate_means(selected_model, by=c("treatment", "hour"))

# ggplot(model.contrasts, aes(estimate, y = hour))+#, label = scales::pvalue(p.value, accuracy = 0.01)))+
#   geom_point(size = 1)+
#   geom_errorbar(aes(x = estimate,
#                     xmin = asymp.LCL,
#                     xmax = asymp.UCL),
#                 width = 0.2,
#                 size = 0.5)+
#   geom_vline(xintercept = 0,
#              linetype = "dashed",
#              color = "red",
#              size = 1,
#              alpha = 1)+
#   theme_bw()+
#   ggtitle("Log odds ratio for correct response\n[CTR / STZ]")+
#   theme(plot.title = element_text(vjust = 0.5, lineheight = 1.2))+
#   ylab("")+
#   xlab("")+
#   # geom_text(nudge_y = 0.25, size = 3.5)+
#   scale_y_continuous(
#     breaks = unique(df_binned_test$hour)+2,
#     labels = function(x) { d = ((x-2) / 24)+1; paste("day", d) }
#   )+
#   facet_wrap(~contrast)
#   # facet_wrap(~factor(hour, labels = paste("day", ((unique(model.contrasts$hour)-2) / 24)+1)))

model.contrasts$hour <- as.numeric(as.character(model.contrasts$hour))
ggplot(model.contrasts, aes(y = odds.ratio, x = hour))+#, label = scales::pvalue(p.value, accuracy = 0.01)))+
  geom_point(size = 1)+
  geom_errorbar(aes(ymin = asymp.LCL,
                    ymax = asymp.UCL),
                width = 0.2,
                size = 0.5)+
  geom_hline(yintercept = 1,
             linetype = "longdash",
             color = "red",
             linewidth = 0.5,
             alpha = 1)+
  theme_bw()+
  ggtitle("Odds ratio for correct response")+
  theme(plot.title = element_text(vjust = 0.5, lineheight = 1.2))+
  ylab("")+
  xlab("Day")+
  # geom_text(nudge_y = 0.25, size = 3.5)+
  scale_y_continuous(transform = "log10",
                     limits = c(0.03, 30),
                     labels = scales::label_number())+
  scale_x_continuous(
    breaks = unique(model.contrasts$hour),
    labels = function(x) { d = ((x-2) / 24)+1}#; paste("Day", d) }
  )+
  facet_wrap(~contrast)
  #theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
ggsave(paste0(working_directory, "/2-2_correct_contrasts.png"), height = 5.5, width = 12, units = "cm")
# facet_wrap(~factor(hour, labels = paste("day", ((unique(model.contrasts$hour)-2) / 24)+1)))


ggplot(data = model.means, aes(x = hour, y = prob)) +
  # geom_violin() +
  # geom_jitter(width = 0.05) +
  # geom_ribbon(color=NA, alpha = 0.1,
  #             aes(ymin = asymp.LCL, ymax = asymp.UCL, fill = treatment)) +
  geom_line(aes(color = treatment), alpha=0.2) +
  geom_pointrange(
    data = model.means %>% filter(hour %in% c(unique(df_test$day)*24+2)),
    aes(y = prob, ymin = asymp.LCL, ymax = asymp.UCL, color = treatment),
    size = 0.8,
    position = position_dodge(5)
    #color = treatment
  ) +
  geom_hline(yintercept = 0.85,
             linetype = "longdash",
             color = "black",
             size = 0.5,
             alpha = 0.5)+
  # stat_summary_bin(data = df_test,
  #                  aes(
  #                    y = correct,
  #                    color = treatment,
  #                    width = after_stat(5)
  #                  ),
  #                  breaks = seq(min(df_binned_test$hour)-12,
  #                               max(df_binned_test$hour)+12,
  #                               by = 24),
  #                  #position = position_dodge(width = 0.1),
  #                  fun.data=mean_se, geom="errorbar") +
  theme_minimal()+
  theme(legend.position = "top", legend.justification='left') +
  scale_x_continuous(
    breaks = unique(df_binned_test$hour),
    labels = function(x) { d = (x/24)+1 }
  ) +
  scale_y_continuous(labels = scales::percent, limits = c(0,1), breaks = c(0, 0.25, 0.5, 0.75, 0.85, 1)) +
  ylab("Probability of correct response") +
  xlab("Day") +
  labs(colour = "Treatment") +
  # ggtitle(deparse1(model.to_plot$modelInfo$allForm$formula))
  ggtitle("Model-estimated learning curve")
  
ggsave(paste0(working_directory, "/2-1_correct_curves.png"), height = 12, width = 15, units = "cm")

model.session_emm <- emmeans(selected_model, ~session_part*treatment|hour, at=list(hour = unique(df_test$day)*24+2))
model.session_emm_overall <- emmeans(selected_model, ~session_part*treatment)

model.session_contrasts.raw <- contrast(model.session_emm, interaction=c("trt.vs.ctrl", "revpairwise"), by="hour")
plot(model.session_contrasts.raw)
session_1 <- c(1,0,0,1,0,0)
session_2 <- c(0,1,0,0,1,0)
session_3 <- c(0,0,1,0,0,1)
ctr <- c(rep(1,3), rep(0,3))
stz <- c(rep(0,3), rep(1,3))
ctr_1 <- as.numeric(ctr & session_1)
stz_1 <- as.numeric(stz & session_1)
ctr_2 <- as.numeric(ctr & session_2)
stz_2 <- as.numeric(stz & session_2)
ctr_3 <- as.numeric(ctr & session_3)
stz_3 <- as.numeric(stz & session_3)
ctr_3v2 <- (ctr_1) - ctr_2
stz_3v2 <- (stz_1) - stz_2
stz_13v2 <- stz_2 - ((stz_1+stz_3)/2)
ctr_13v2 <- ctr_2 - ((ctr_1+ctr_3)/2)

model.session.contrasts_overall <- model.session_emm_overall %>% 
  contrast(
    method = list(
      "(STZ 3 - 2) - (CTR 3 - 2)" = stz_3v2-ctr_3v2
    )
  )

model.session_emm_athour <- emmeans(selected_model, ~session_part*treatment|hour, at=list(hour = 98))
model.session.contrasts_overall <- model.session_emm %>% 
  contrast(
    method = list(
      "CTR 3 v 1" = ctr_3 - ctr_1,
      "STZ 3 v 1" = stz_3 - stz_1,
      "CTR 1&3 v 2" = ctr_13v2,
      "STZ 1&3 v 2" = stz_13v2
      # "STZ v CTR" = stz_13v2-ctr_13v2
    ),
    # adjust="sidak",
    infer=c(T,T)
  )

model.session.contrasts_overall <- model.session_emm %>% 
  contrast(
    method = list(
      "CTR 1 v 2" = ctr_2 - ctr_1,
      "CTR 3 v 2" = ctr_2 - ctr_3,
      "STZ 1 v 2" = stz_2 - stz_1,
      "STZ 3 v 2" = stz_2 - stz_3,
      "CTR 1&3 v 2" = ctr_13v2,
      "STZ 1&3 v 2" = stz_13v2
      # "STZ v CTR" = stz_13v2-ctr_13v2
    )
  )

# model.session_contrasts.raw <- emmeans(t3.f.xr0h3, consec~session_part|hour|treatment, at=list(hour = unique(df_test$day)*24+2))
# model.session_contrasts <- (model.session_contrasts %>% summary(by = NULL, infer=c(TRUE,TRUE), adjust="none"))$contrast
model.session_means <- (
  emmeans(
    selected_model,
    ~treatment|session_part|hour,
    # at = list(hour = seq(min(df_test$hour),
    #                      max(df_test$hour),
    #                      0.5)
    # ),
    at = list(hour = unique(df_test$day)*24),
    type = "response"
    #adjust = "none"
  ) %>% 
    summary(by = NULL)
)

model.session.means.overall <- emmeans(
  selected_model,
  ~session_part*treatment|hour,
  # at = list(hour = seq(min(df_test$hour),
  #                      max(df_test$hour),
  #                      0.5)
  # ),
  #at = list(hour = unique(df_test$day)*24),
  #at = list(hour = 26)
  #type = "response"
  #adjust = "none"
)

model.session.contrasts.overall <- model.session.means.overall %>% 
  contrast(
    method = list(
      "CTR, S3 - S1" = ctr_3-ctr_1,
      "CTR, S2 - S1" = ctr_2-ctr_1,
      "CTR, S2 - S3" = ctr_2-ctr_3,
      "STZ, S3 - S1" = stz_3-stz_1,
      "STZ, S2 - S1" = stz_2-stz_1,
      "STZ, S2 - S3" = stz_2-stz_3,
      "STZ - CTR: S3 - S1" = (stz_3-stz_1)-(ctr_3-ctr_1),
      "STZ - CTR: S2 - S1" = (stz_2-stz_1)-(ctr_2-ctr_1),
      "STZ - CTR: S2 - S3" = (stz_2-stz_3)-(ctr_2-ctr_3)
    ),
    infer = c(T,T)
  )

plot_effect_with_ci(as.data.frame(model.session.contrasts.overall), p.value = T)# + xlim(-1,2)

model.session_means$session_part <- as.factor(model.session_means$session_part)

ggplot(data = model.session_means,# %>% filter(treatment == "CTR", session_part == 1),
       aes(x = hour, y = prob, group = interaction(treatment, session_part))) +
  # geom_violin() +
  # geom_jitter(width = 0.05) +
  # geom_ribbon(color=NA, alpha = 0.1,
  #             aes(ymin = asymp.LCL, ymax = asymp.UCL, fill = treatment)) +
  geom_ribbon(
    data = model.session_means %>% filter(hour %in% c(unique(df_test$day)*24)),
    aes(y = prob, ymin = asymp.LCL, ymax = asymp.UCL, fill = session_part), alpha = 0.1,
    #position = position_dodge(10),
    #size = 0.3
  ) +
  geom_line(aes(color = session_part))+#, linetype = session_part)) +
  geom_point(
    data = model.session_means %>%
      filter(hour %in% c(unique(df_test$day)*24)),# %>%
    # filter(treatment == "CTR", session_part == 1),
    aes(y = prob, color = session_part, shape = session_part),
    #position = position_dodge(10),
    size = 2.5
  ) +

  # stat_summary_bin(data = df_test,
  #                  aes(
  #                    y = correct,
  #                    color = treatment,
  #                    width = after_stat(5)
  #                  ),
  #                  breaks = seq(min(df_binned_test$hour)-12,
  #                               max(df_binned_test$hour)+12,
  #                               by = 24),
  #                  #position = position_dodge(width = 0.1),
  #                  fun.data=mean_se, geom="errorbar") +
  theme_minimal() +
  theme(legend.position = "top", legend.justification = "left") +
  scale_x_continuous(
    breaks = unique(df_binned_test$hour),
    labels = function(x) { d = (x/24)+1 }
  ) +
  scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
  ylab("Probability of correct response") +
  xlab("Day") +
  labs(fill = "Session", shape = "Session", colour = "Session") +
  scale_color_brewer(palette = "Set2") +
  scale_fill_brewer(palette = "Set2") +
  # ggtitle(deparse1(model.to_plot$modelInfo$allForm$formula))
  ggtitle("Model-estimated learning curve")+
  facet_wrap(~treatment)
# ggsave(paste0(working_directory, "/2-3_correct_session_curves.png"), height = 8, width = 19, units =  "cm")
ggsave(paste0(working_directory, "/2-3_correct_session_curves.png"), height = 11, width = 19, units = "cm")


#### CORRECT_PCT - TEST DIFFICULT ####
df_test <- df_full %>%
  filter(protocol == "test") %>% 
  filter(difficulty == "difficult")
df_test <- df_test %>% filter(!is.na(correct))
df_test$hour <- as.numeric(df_test$t_start-min(df_test$t_start))/3600
df_test$day <- df_test$day-min(df_test$day)

df_binned_test <- df_full_binned %>% 
  filter(protocol == "test") %>% 
  filter(difficulty == "difficult")
df_binned_test$correct_pct <- df_binned_test$correct/df_binned_test$precue_correct
df_binned_test$hour <- as.numeric(df_binned_test$t_start-min(df_binned_test$t_start))/3600
df_binned_test$day <- df_binned_test$day-min(df_binned_test$day)

# # za test difficult
# # let's check for odd readings
# df_test %>% dplyr::select(t_start, animal, impulsive_nosepokes) %>% arrange(desc(impulsive_nosepokes))
# # we have a suspiciously high number of impulsive nosepokes for `crveni9` on the 0th day. let's remove that day for that animal
# df_test <- df_test %>% filter(!((animal == "crveni9") & (day == 0)))
# df_binned_test <- df_binned_test %>% filter(!((animal == "crveni9") & (day == 0)))

ggplot(df_binned_test,
       aes(x=day, y=correct_pct, color=treatment)) +
  stat_summary_bin(geom="point", fun=mean, breaks = seq(min(df_test$day)-0.5, max(df_test$day)+0.5)) +
  stat_summary_bin(geom="errorbar", aes(width = after_stat(0)), fun.data=mean_se, breaks = seq(min(df_test$day)-0.5, max(df_test$day)+0.5)) +
  theme_bw(base_size=10)+
  theme(panel.grid.minor = element_blank())+
  scale_x_continuous(breaks=seq(min(df_test$day), max(df_test$day)))+
  ggtitle("Each day as a mean of every animal's daily mean")
ggplot(df_test %>% filter(day > 0),
       aes(x=day, y=correct, color=treatment)) +
  stat_summary_bin(geom="point", fun=mean, breaks = seq(min(df_test$day)-0.5, max(df_test$day)+0.5)) +
  stat_summary_bin(geom="errorbar", aes(width = after_stat(0)), fun.data=mean_se, breaks = seq(min(df_test$day)-0.5, max(df_test$day)+0.5)) +
  theme_bw(base_size=10)+
  theme(panel.grid.minor = element_blank())+
  scale_x_continuous(breaks=seq(min(df_test$day), max(df_test$day)))+
  ggtitle("Each day as a mean of all animals' trials")
#  facet_wrap(~session_part)


# Both the grand mean and mean of means show the highest performance on day 6, after which it drops off,
# likely due to something happening outside the cage - perhaps behaviour,  though unlikely (large drop in
# performance on the day of open field (day 7), better performance on days of NOR (days 7 and 8)).
# As we'll be analysing the learning rate using curve-fitting, days after day 6 will only introduce noise
# the learning process, which we're interested in, is practically finished, i.e. the drop in performance
# after day 6 isn't due to (a failure of) learning, but curve-fitting would try to account for this data
# as well. Hence, we will limit curve-fitting to days 0-6.
# df_test <- df_test %>% filter(day %in% 0:6)
# df_binned_test <- df_binned_test %>% filter(day %in% 0:6)

# vizdata <- estimate_relation(m.2t)
# ggplot(vizdata, aes(x = hour, y = Predicted, color=treatment)) +
#   geom_ribbon(aes(ymin = CI_low, ymax = CI_high), alpha = 0.1) +
#   geom_line() +
#   # Add original data points
#   geom_point(data = df_binned_test, aes(x = hour, y = correct_pct)) +
#   # Aesthetics
#   theme_minimal()


selected_model <- glmmTMB(correct ~
                            poly(hour,2)*as.factor(treatment)*as.factor(session_part) +
                            (hour | animal)+
                            (1 | session_part:day:animal),
                          data = df_test,
                          family = binomial(),
                          control = glmmTMBControl(parallel = parallel::detectCores()))
# selected_model.f <- glmmTMB(cbind(correct, precue_correct-correct) ~
#                             poly(hour,3)*as.factor(treatment)*as.factor(session_part) +
#                             (hour | animal)+
#                             (0+hour | animal:day:session_part),
#                           data = df_test %>% filter(session_part %in% c(2,3)),
#                           family = binomial(link = "probit"),
#                           control = glmmTMBControl(parallel = parallel::detectCores()))

ggplot(data = df_test, aes(x = hour, color = session_part, group = session_part)) +
  stat_summary_bin(data = df_test,
                   aes(x = hour, y = correct, color = session_part),
                   fun=mean,
                   geom="point",
                   breaks = c(0, seq(12,240,24))) +
  stat_summary_bin(data = df_test,
                   aes(x = hour, y = correct, color = session_part),
                   fun.data=mean_se,
                   geom="errorbar",
                   breaks = c(0, seq(12,240,24))) +
  stat_summary_bin(aes(y=fitted(selected_model), color = session_part),
                   breaks = c(0, seq(12,240,24)),
                   fun=mean, geom="line") +
  theme_bw(base_size=10) +
  #scale_y_sqrt()+
  facet_wrap(~animal*session_part)

model.contrasts.raw <- emmeans(selected_model, pairwise~treatment|hour, at=list(hour = unique(df_test$day)*24+2))
model.contrasts <- (model.contrasts.raw %>% summary(by = NULL, infer=c(TRUE,TRUE), adjust="sidak"))$contrast
# model.means <- estimate_means(selected_model, by=c("treatment", "hour"))
model.means <- (
  emmeans(
    selected_model,
    ~treatment|hour,
    at = list(hour = seq(min(df_test$hour),
                         max(df_test$hour),
                         0.5)
    ),
    # at = list(hour = unique(df_test$day)/24),
    type = "response",
    adjust = "sidak"
  ) %>% 
    summary(by = NULL)
)
ggplot(model.contrasts, aes(estimate, y = hour, label = scales::pvalue(p.value, accuracy = 0.001)))+
  geom_point(size = 2)+
  geom_errorbar(aes(x = estimate,
                    xmin = asymp.LCL,
                    xmax = asymp.UCL),
                width = 0.3,
                size = 1)+
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red",
             size = 1,
             alpha = 1)+
  theme_bw()+
  ggtitle("Log odds ratio for correct response\n[CTR / STZ]")+
  theme(plot.title = element_text(vjust = 0.5, lineheight = 1.2))+
  ylab("")+
  xlab("")+
  geom_text(nudge_y = 10, size = 3.5)+
  scale_y_continuous(
    breaks = unique(df_binned_test$hour)+2,
    labels = function(x) { d = ((x-2) / 24)+1; paste("day", d) }
  )


ggplot(data = model.means, aes(x = hour, y = prob)) +
  # geom_violin() +
  # geom_jitter(width = 0.05) +
  # geom_ribbon(color=NA, alpha = 0.1,
  #             aes(ymin = asymp.LCL, ymax = asymp.UCL, fill = treatment)) +
  geom_line(aes(color = treatment), alpha=0.2) +
  geom_pointrange(
    data = model.means %>% filter(hour %in% c(unique(df_test$day)*24)),
    aes(y = prob, ymin = asymp.LCL, ymax = asymp.UCL, color = treatment),
    size = 1
    # position = position_dodge(0)
    #color = treatment
  ) +
  # stat_summary_bin(data = df_test,
  #                  aes(
  #                    y = correct,
  #                    color = treatment,
  #                    width = after_stat(5)
  #                  ),
  #                  breaks = seq(min(df_binned_test$hour)-12,
  #                               max(df_binned_test$hour)+12,
  #                               by = 24),
  #                  #position = position_dodge(width = 0.1),
  #                  fun.data=mean_se, geom="errorbar") +
  theme_bw() +
  scale_x_continuous(
    breaks = unique(df_binned_test$hour),
    labels = function(x) { d = (x/24)+1 }
  ) +
  scale_y_continuous(labels = scales::percent) +
  ylab("Probability of correct response") +
  xlab("Day") +
  # ggtitle(deparse1(model.to_plot$modelInfo$allForm$formula))
  ggtitle("Model-estimated learning curve")

model.session_emm.trt_vs_ctrl <- emmeans(selected_model, ~session_part|hour|treatment, at=list(hour = unique(df_test$day)*24+2))
model.session_contrasts.within_group.raw <- contrast(model.session_emm, method="trt.vs.ctrl", by=c("treatment", "hour"))
model.session_contrasts.raw <- contrast(model.session_emm, interaction=c("trt.vs.ctrl", "revpairwise"), by="hour")
plot(model.session_contrasts.within_group.raw)
plot(model.session_contrasts.raw)

# model.session_contrasts.raw <- emmeans(t3.f.xr0h3, consec~session_part|hour|treatment, at=list(hour = unique(df_test$day)*24+2))
# model.session_contrasts <- (model.session_contrasts %>% summary(by = NULL, infer=c(TRUE,TRUE), adjust="none"))$contrast
model.session_means <- (
  emmeans(
    selected_model,
    ~treatment|session_part|hour,
    # at = list(hour = seq(min(df_test$hour),
    #                      max(df_test$hour),
    #                      0.5)
    # ),
    at = list(hour = unique(df_test$day)*24),
    type = "response",
    adjust = "none"
  ) %>% 
    summary(by = NULL)
)

model.session_means$session_part <- as.factor(model.session_means$session_part)

ggplot(data = model.session_means,# %>% filter(treatment == "CTR", session_part == 1),
       aes(x = hour, y = prob, group = interaction(treatment, session_part))) +
  # geom_violin() +
  # geom_jitter(width = 0.05) +
  # geom_ribbon(color=NA, alpha = 0.1,
  #             aes(ymin = asymp.LCL, ymax = asymp.UCL, fill = treatment)) +
  geom_ribbon(
    data = model.session_means %>% filter(hour %in% c(unique(df_test$day)*24)),
    aes(y = prob, ymin = asymp.LCL, ymax = asymp.UCL, fill = session_part), alpha = 0.1,
    #position = position_dodge(10),
    #size = 0.3
  ) +
  geom_line(aes(color = session_part))+#, linetype = session_part)) +
  geom_point(
    data = model.session_means %>%
      filter(hour %in% c(unique(df_test$day)*24)),# %>%
    # filter(treatment == "CTR", session_part == 1),
    aes(y = prob, color = session_part, shape = session_part),
    #position = position_dodge(10),
    size = 3
  ) +
  
  # stat_summary_bin(data = df_test,
  #                  aes(
  #                    y = correct,
  #                    color = treatment,
  #                    width = after_stat(5)
  #                  ),
  #                  breaks = seq(min(df_binned_test$hour)-12,
  #                               max(df_binned_test$hour)+12,
  #                               by = 24),
  #                  #position = position_dodge(width = 0.1),
  #                  fun.data=mean_se, geom="errorbar") +
  theme_minimal() +
  scale_x_continuous(
    breaks = unique(df_binned_test$hour),
    labels = function(x) { d = (x/24)+1 }
  ) +
  scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
  ylab("Probability of correct response") +
  xlab("Day") +
  scale_color_brewer(palette = "Set2") +
  scale_fill_brewer(palette = "Set2") +
  # ggtitle(deparse1(model.to_plot$modelInfo$allForm$formula))
  ggtitle("Model-estimated learning curve")+
  facet_wrap(~treatment)

#### NUMBER OF TRIALS - TEST EASY ####
df_test <- df_full %>%
  filter(protocol == "test") %>% 
  filter(difficulty == "easy")
df_test <- df_test %>% filter(!is.na(correct))
df_test$hour <- as.numeric(df_test$t_start-min(df_test$t_start))/3600
df_test$day <- df_test$day-min(df_test$day)

df_binned_test <- df_full_binned %>% 
  filter(protocol == "test") %>% 
  filter(difficulty == "easy")
df_binned_test$correct_pct <- df_binned_test$correct/df_binned_test$precue_correct
df_binned_test$hour <- as.numeric(df_binned_test$t_start-min(df_binned_test$t_start))/3600
df_binned_test$day <- df_binned_test$day-min(df_binned_test$day)

# # za test difficult
# # let's check for odd readings
# df_test %>% dplyr::select(t_start, animal, impulsive_nosepokes) %>% arrange(desc(impulsive_nosepokes))
# # we have a suspiciously high number of impulsive nosepokes for `crveni9` on the 0th day. let's remove that day for that animal
# df_test <- df_test %>% filter(!((animal == "crveni9") & (day == 0)))
# df_binned_test <- df_binned_test %>% filter(!((animal == "crveni9") & (day == 0)))

ggplot(df_binned_test,
       aes(x=day, y=precue_correct, color=treatment)) +
  stat_summary_bin(geom="point", fun=sum, breaks = seq(min(df_test$day)-0.5, max(df_test$day)+0.5)) +
  #stat_summary_bin(geom="errorbar", aes(width = after_stat(0)), fun.data=mean_se, breaks = seq(min(df_test$day)-0.5, max(df_test$day)+0.5)) +
  theme_bw(base_size=10)+
  theme(panel.grid.minor = element_blank())+
  scale_x_continuous(breaks=seq(min(df_test$day), max(df_test$day)))+
  ggtitle("Each day as a mean of every animal's daily mean")
ggplot(df_test %>% filter(day > 0),
       aes(x=day, y=precue_correct, color=treatment)) +
  stat_summary_bin(geom="point", fun=sum, breaks = seq(min(df_test$day)-0.5, max(df_test$day)+0.5)) +
  #stat_summary_bin(geom="errorbar", aes(width = after_stat(0)), fun.data=mean_se, breaks = seq(min(df_test$day)-0.5, max(df_test$day)+0.5)) +
  theme_bw(base_size=10)+
  theme(panel.grid.minor = element_blank())+
  scale_x_continuous(breaks=seq(min(df_test$day), max(df_test$day)))+
  ggtitle("Each day as a mean of all animals' trials")
#  facet_wrap(~session_part)


# Both the grand mean and mean of means show the highest performance on day 6, after which it drops off,
# likely due to something happening outside the cage - perhaps behaviour,  though unlikely (large drop in
# performance on the day of open field (day 7), better performance on days of NOR (days 7 and 8)).
# As we'll be analysing the learning rate using curve-fitting, days after day 6 will only introduce noise
# the learning process, which we're interested in, is practically finished, i.e. the drop in performance
# after day 6 isn't due to (a failure of) learning, but curve-fitting would try to account for this data
# as well. Hence, we will limit curve-fitting to days 0-6.
# df_test <- df_test %>% filter(day %in% 0:6)
# df_binned_test <- df_binned_test %>% filter(day %in% 0:6)

# vizdata <- estimate_relation(m.2t)
# ggplot(vizdata, aes(x = hour, y = Predicted, color=treatment)) +
#   geom_ribbon(aes(ymin = CI_low, ymax = CI_high), alpha = 0.1) +
#   geom_line() +
#   # Add original data points
#   geom_point(data = df_binned_test, aes(x = hour, y = correct_pct)) +
#   # Aesthetics
#   theme_minimal()

df_binned_test_overall <- df_binned_test %>% filter(session_part == "overall")
selected_model <- glmmTMB(precue_correct ~
                            poly(day,2)*as.factor(treatment) +
                            as.factor(treatment)*as.factor(bhv_performed)+
                            (day | animal),
                            #(1 | session_part:day:animal),
                          # (poly(day,2) | animal),
                          # (0+day | session_part:day:animal),
                          data = df_binned_test_overall,
                          family = nbinom1(),
                          control = glmmTMBControl(parallel = parallel::detectCores()))

model_performance(selected_model)
check_overdispersion(selected_model)
check_zeroinflation(selected_model)
check_collinearity(selected_model)
check_autocorrelation(selected_model)
sim.res <- simulateResiduals(selected_model, n = 1000, plot = F)
plot(sim.res)

ggplot(data = df_binned_test_overall, aes(x = day, color = as.factor(session_part), group = session_part)) +
  stat_summary_bin(data = df_binned_test_overall,
                   aes(x = day, y = precue_correct, color = session_part),
                   fun=mean,
                   geom="point")+
  #breaks = c(0, seq(12,240,24))) +
  stat_summary_bin(data = df_binned_test_overall,
                   aes(x = day, y = precue_correct, color = session_part),
                   fun.data=mean_se,
                   geom="errorbar")+
  #breaks = c(0, seq(12,240,24))) +
  stat_summary_bin(aes(y=fitted(selected_model), color = session_part),
                   #breaks = c(0, seq(12,240,24)),
                   fun=mean, geom="line") +
  theme_bw(base_size=10) +
  #scale_y_sqrt()+
  facet_wrap(~animal*session_part)

model.contrasts.raw <- emmeans(selected_model, pairwise~treatment|day, at = list(day = unique(df_binned_test$day)))
model.contrasts <- (model.contrasts.raw %>% summary(by = NULL, infer=c(TRUE,TRUE), adjust="sidak"))$contrast
# model.means <- estimate_means(selected_model, by=c("treatment", "hour"))
model.means.raw.overall <- (
  emmeans(
    selected_model,
    ~treatment*day,
    type = "response"
  )
)
model.means <- (
  emmeans(
    selected_model,
    ~treatment*day,
    # at = list(hour = seq(min(df_test$hour),
    #                      max(df_test$hour),
    #                      0.5)
    # ),
    at = list(day = unique(df_test$day)),
    type = "response"
  ) %>% 
    summary(by = NULL)
)
model.means
model.contrasts$day <- as.factor(model.contrasts$day+1)
plot_effect_with_ci(as.data.frame(model.contrasts), y_var = "day", title = "Difference in the number of started trials\n[CTR - STZ]", transpose = T)
ggsave(paste0(working_directory, "/3-2_n-trials_contrasts.png"), height = 8, width = 15, units = "cm")

ntrialsplot <- ggplot(data = model.means, aes(x = day, y = response)) +
  geom_line(aes(color = treatment), alpha=0.2) +
  geom_pointrange(
    data = model.means,
    aes(y = response, ymin = asymp.LCL, ymax = asymp.UCL, color = treatment),
    size = 0.8,
    position = position_dodge(0.2)
  ) +
  theme_minimal()+
  theme(legend.position = "top", legend.justification='left') +
  scale_x_continuous(
    breaks = unique(df_binned_test$day),
    labels = function(x) { d = x+1 }
  ) +
  ylab("Number of started trials") +
  xlab("Day") +
  labs(color = "Treatment")+
  ggtitle("Model-estimated number of started trials")
  # scale_colour_brewer(palette = "Set1")
ntrialsplot
ggsave(paste0(working_directory, "/3-1_n-trials_curves.png"), height = 12, width = 15, units = "cm")

df_binned_test_sess <- df_binned_test %>% filter(session_part != "overall")
selected_model_sess <- glmmTMB(precue_correct ~
                                 poly(day,2)*as.factor(treatment)*as.factor(session_part) +
                                 as.factor(treatment)*as.factor(bhv_performed)+
                                 (1 | animal)+
                                 (1 | session_part:day:animal),
                               # (poly(day,2) | animal),
                               # (0+day | session_part:day:animal),
                               data = df_binned_test_sess,
                               family = nbinom2(),
                               control = glmmTMBControl(parallel = parallel::detectCores()))
model_performance(selected_model_sess)
check_overdispersion(selected_model_sess)
check_zeroinflation(selected_model_sess)
check_collinearity(selected_model_sess)
check_autocorrelation(selected_model_sess)
sim.res <- simulateResiduals(selected_model_sess, n = 1000, plot = F)
plot(sim.res)

model.session_means <- emmeans(selected_model_sess, ~session_part*day*treatment, at=list(day = unique(df_binned_test$day)))

# model.session_means <- emmeans(selected_model_sess, ~day|treatment, at=list(hour = unique(df_test$day)))
model.session_contrasts.within_group.raw <- contrast(model.session_means, method="trt.vs.ctrl", by=c("treatment", "day"))
plot(model.session_contrasts.within_group.raw)

ggplot(data = as.data.frame(model.session_means),# %>% filter(treatment == "CTR", session_part == 1),
       aes(x = day, y = emmean, group = session_part)) +
  geom_ribbon(
    aes(y = emmean, ymin = asymp.LCL, ymax = asymp.UCL, fill = session_part), alpha = 0.1,
  ) +
  geom_line(aes(color = session_part))+#, linetype = session_part)) +
  geom_point(
    aes(y = emmean, color = session_part, shape = session_part),
    #position = position_dodge(10),
    size = 2.5
  ) +
  theme_minimal() +
  theme(legend.position = "top", legend.justification = "left") +
  scale_x_continuous(
    breaks = unique(as.data.frame(model.session_means)$day),
    labels = function(x) { d = x+1 }
  ) +
  # scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
  ylab("Number of trials") +
  xlab("Day") +
  scale_color_brewer(palette = "Set2") +
  scale_fill_brewer(palette = "Set2") +
  # ggtitle(deparse1(model.to_plot$modelInfo$allForm$formula))
  ggtitle("Number of trials per day in each session")+
  labs(shape = "Session", fill = "Session", colour = "Session")+
  facet_wrap(~treatment)

# ggsave(paste0(working_directory, "/2-3_correct_session_curves.png"), height = 8, width = 19, units =  "cm")
# ggsave(paste0(working_directory, "/2-3_correct_session_curves.png"), height = 12, width = 19, units = "cm")
ggsave(paste0(working_directory, "/3-3_n-trials_session_curves.png"), height = 12, width = 19, units = "cm")

model.bhv_emm.raw <- emmeans(selected_model, ~bhv_performed*treatment, type="response")
model.bhv_emm <- model.bhv_emm.raw %>% summary(by = NULL)
model.bhv_emm$bhv_performed <- as.logical(model.bhv_emm$bhv_performed)
model.bhv_emm$treatment <- as.factor(model.bhv_emm$treatment)
bhvplot <- ggplot(data = model.bhv_emm,# %>% filter(treatment == "CTR", session_part == 1),
                  aes(x = treatment, y = response, color = bhv_performed)) +
  geom_point(size = 3, position = position_dodge(0.5))+
  geom_errorbar(aes(y=response, ymin=asymp.LCL, ymax=asymp.UCL), width=0, position = position_dodge(0.5))+
  theme_minimal()+
  theme(legend.position = "top",
        legend.justification = "left",
        legend.margin = margin(t = 0, r = 0, b = 0, l = -30, unit = "pt"),
        plot.title = element_text(size=11, margin = margin(t = 0, r = 0, b = 0, l = -30, unit = "pt")),
        ) +
  xlab("Treatment")+
  ylab("Number of trials per night")+
  guides(color = guide_legend(title = ""))+
  ggtitle("Out-of-cage testing\nperformed")+
  scale_colour_brewer(palette = "Set2")
# scale_y_continuous(trans = "log10", breaks = c(3, 5, 7, 10, 15))
bhvplot
ggsave(paste0(working_directory, "/4-bhv.png"), height = 20, width = 12, units = "cm")

library(patchwork)
set2_colors12 <- RColorBrewer::brewer.pal(4, "Set2")
set2_colors34 <- RColorBrewer::brewer.pal(4, "Set2")[3:4]
ntrialsplot + scale_colour_manual(values = set2_colors12) +
  plot_spacer() +
  bhvplot + scale_colour_manual(values = set2_colors34) +
  plot_spacer() + plot_layout(widths = c(5,0.1,1,0.5))
ggsave(paste0(working_directory, "/3-4-composite.png"), height = 12, width = 20, units = "cm")


# Does the behaviour affect the animals, stratified by CTR and STZ
model.bhv_emm.raw <- emmeans(selected_model, ~bhv_performed|treatment)

contrast(model.bhv_emm.raw, "revpairwise", infer = c(T,T), type="response") %>% summary(by=NULL)
model.bhv_emm.raw.all <- emmeans(selected_model, ~bhv_performed*treatment)
contrast(model.bhv_emm.raw.all, interaction=c("revpairwise", "revpairwise"),type="response", infer = c(T,T))

# Difference of differences - does the behaviour affect CTR and STZ differently.
# If negative, STZ is less affected by behaviour
model.bhv_emm.raw <- emmeans(selected_model, ~bhv_performed|treatment)
contrast(model.bhv_emm.raw, interaction=TRUE, "revpairwise")

model.session_means <- (
  emmeans(
    selected_model_sess,
    ~treatment|session_part|day,
    # at = list(hour = seq(min(df_test$hour),
    #                      max(df_test$hour),
    #                      0.5)
    # ),
    at = list(day = unique(df_test$day)),
    type = "response",
    adjust = "none"
  ) %>%
    summary(by = NULL)
)

model.session_means$session_part <- as.factor(model.session_means$session_part)

ggplot(data = model.session_means,
       aes(x = day, y = rate, group = interaction(treatment, session_part))) +
  geom_ribbon(
    data = model.session_means,
    aes(y = rate, ymin = asymp.LCL, ymax = asymp.UCL, fill = session_part), alpha = 0.1
  ) +
  geom_line(aes(color = session_part))+
  geom_point(
    data = model.session_means,
    aes(y = rate, color = session_part, shape = session_part),
    size = 3
  ) +
  theme_minimal() +
  scale_x_continuous(
    breaks = unique(df_binned_test$day),
    labels = function(x) { d = x+1 }
  ) +
  ylab("Number of started trials") +
  xlab("Day") +
  scale_color_brewer(palette = "Set2") +
  scale_fill_brewer(palette = "Set2") +
  ggtitle("Model-estimated number of trials over time [test easy]")+
  facet_wrap(~treatment)


#### NUMBER OF TRIALS - TEST DIFFICULT ####
df_test <- df_full %>%
  filter(protocol == "test") %>% 
  filter(difficulty == "difficult")
df_test <- df_test %>% filter(!is.na(correct))
df_test$hour <- as.numeric(df_test$t_start-min(df_test$t_start))/3600
df_test$day <- df_test$day-min(df_test$day)

df_binned_test <- df_full_binned %>% 
  filter(protocol == "test") %>% 
  filter(difficulty == "difficult")
df_binned_test$correct_pct <- df_binned_test$correct/df_binned_test$precue_correct
df_binned_test$hour <- as.numeric(df_binned_test$t_start-min(df_binned_test$t_start))/3600
df_binned_test$day <- df_binned_test$day-min(df_binned_test$day)

df_binned_test %>% dplyr::select(t_start, animal, precue_correct) %>% arrange(desc(precue_correct))
# # za test difficult
# # let's check for odd readings
# df_test %>% dplyr::select(t_start, animal, impulsive_nosepokes) %>% arrange(desc(impulsive_nosepokes))
# # we have a suspiciously high number of impulsive nosepokes for `crveni9` on the 0th day. let's remove that day for that animal
# df_test <- df_test %>% filter(!((animal == "crveni9") & (day == 0)))
# df_binned_test <- df_binned_test %>% filter(!((animal == "crveni9") & (day == 0)))

ggplot(df_binned_test,
       aes(x=day, y=precue_correct, color=treatment)) +
  stat_summary_bin(geom="point", fun=sum, breaks = seq(min(df_test$day)-0.5, max(df_test$day)+0.5)) +
  #stat_summary_bin(geom="errorbar", aes(width = after_stat(0)), fun.data=mean_se, breaks = seq(min(df_test$day)-0.5, max(df_test$day)+0.5)) +
  theme_bw(base_size=10)+
  theme(panel.grid.minor = element_blank())+
  scale_x_continuous(breaks=seq(min(df_test$day), max(df_test$day)))+
  ggtitle("Each day as a mean of every animal's daily mean")
ggplot(df_test %>% filter(day > 0),
       aes(x=day, y=precue_correct, color=treatment)) +
  stat_summary_bin(geom="point", fun=sum, breaks = seq(min(df_test$day)-0.5, max(df_test$day)+0.5)) +
  #stat_summary_bin(geom="errorbar", aes(width = after_stat(0)), fun.data=mean_se, breaks = seq(min(df_test$day)-0.5, max(df_test$day)+0.5)) +
  theme_bw(base_size=10)+
  theme(panel.grid.minor = element_blank())+
  scale_x_continuous(breaks=seq(min(df_test$day), max(df_test$day)))+
  ggtitle("Each day as a mean of all animals' trials")
#  facet_wrap(~session_part)


# Both the grand mean and mean of means show the highest performance on day 6, after which it drops off,
# likely due to something happening outside the cage - perhaps behaviour,  though unlikely (large drop in
# performance on the day of open field (day 7), better performance on days of NOR (days 7 and 8)).
# As we'll be analysing the learning rate using curve-fitting, days after day 6 will only introduce noise
# the learning process, which we're interested in, is practically finished, i.e. the drop in performance
# after day 6 isn't due to (a failure of) learning, but curve-fitting would try to account for this data
# as well. Hence, we will limit curve-fitting to days 0-6.
# df_test <- df_test %>% filter(day %in% 0:6)
# df_binned_test <- df_binned_test %>% filter(day %in% 0:6)

# vizdata <- estimate_relation(m.2t)
# ggplot(vizdata, aes(x = hour, y = Predicted, color=treatment)) +
#   geom_ribbon(aes(ymin = CI_low, ymax = CI_high), alpha = 0.1) +
#   geom_line() +
#   # Add original data points
#   geom_point(data = df_binned_test, aes(x = hour, y = correct_pct)) +
#   # Aesthetics
#   theme_minimal()

df_binned_test_overall <- df_binned_test %>% filter(session_part == "overall") %>% filter(difficulty == "difficult")
selected_model_overall <- glmmTMB(precue_correct ~
                                 day*as.factor(treatment) +
                                 as.factor(treatment)*as.factor(bhv_performed)+
                                 (1 | animal),
                               data = df_binned_test_overall,
                               family = nbinom2(),
                               control = glmmTMBControl(parallel = parallel::detectCores()))
model_performance(selected_model_overall)
check_overdispersion(selected_model_overall)
check_zeroinflation(selected_model_overall)
check_collinearity(selected_model_overall)
check_autocorrelation(selected_model_overall)
sim.res <- simulateResiduals(selected_model_overall, n = 1000, plot = F)
plot(sim.res)

ggplot(data = df_binned_test_overall, aes(x = day, color = as.factor(session_part), group = session_part)) +
  stat_summary_bin(data = df_binned_test_overall,
                   aes(x = day, y = precue_correct, color = session_part),
                   fun=mean,
                   geom="point")+
  #breaks = c(0, seq(12,240,24))) +
  stat_summary_bin(data = df_binned_test_overall,
                   aes(x = day, y = precue_correct, color = session_part),
                   fun.data=mean_se,
                   geom="errorbar")+
  #breaks = c(0, seq(12,240,24))) +
  stat_summary_bin(aes(y=fitted(selected_model_overall), color = session_part),
                   #breaks = c(0, seq(12,240,24)),
                   fun=mean, geom="line") +
  theme_bw(base_size=10) +
  #scale_y_sqrt()+
  facet_wrap(~animal*session_part)

model.contrasts.raw <- emmeans(selected_model_overall, pairwise~treatment|day, at = list(day = unique(df_binned_test$day)))
model.contrasts <- (model.contrasts.raw %>% summary(by = NULL, infer=c(TRUE,TRUE), adjust="sidak"))$contrast
model.contrasts.overall <- emmeans(selected_model_overall, pairwise~treatment)$contrast
model.means <- (
  emmeans(
    selected_model_overall,
    ~treatment|day,
    at = list(day = unique(df_test$day)),
    type = "response"
  ) %>% 
    summary(by = NULL)
)
plot_effect_with_ci(as.data.frame(model.contrasts), y_var = "day", title = "Difference in the number of started trials\n[CTR - STZ]", transpose = T)

ggplot(data = model.means, aes(x = day, y = response)) +
  geom_line(aes(color = treatment), alpha=0.2) +
  geom_pointrange(
    data = model.means,
    aes(y = response, ymin = asymp.LCL, ymax = asymp.UCL, color = treatment),
    size = 1
  ) +
  theme_bw() +
  scale_x_continuous(
    breaks = unique(df_binned_test$day),
    labels = function(x) { d = x+1 }
  ) +
  # scale_y_continuous(labels = scales::percent) +
  ylab("Number of started trials") +
  xlab("Day") +
  ggtitle("Model-estimated number of started trials, session-averaged [test difficult]")

model.bhv_emm.raw <- emmeans(selected_model_overall, ~bhv_performed*treatment)
model.bhv_emm <- model.bhv_emm.raw %>% summary(by = NULL)
model.bhv_emm$bhv_performed <- as.logical(model.bhv_emm$bhv_performed)
model.bhv_emm$treatment <- as.factor(model.bhv_emm$treatment)
ggplot(data = model.bhv_emm,# %>% filter(treatment == "CTR", session_part == 1),
       aes(x = treatment, y = emmean, color = bhv_performed)) +
  geom_point(position = position_dodge(0.3))+
  geom_errorbar(aes(y=emmean, ymin=asymp.LCL, ymax=asymp.UCL), width=0.1, position = position_dodge(0.3))+
  theme_minimal()

# Difference of differences - does the behaviour affect CTR and STZ differently.
# If negative, STZ is less affected by behaviour
model.bhv_emm.raw <- emmeans(selected_model_overall, ~bhv_performed*treatment)
bhv_true <- c(0,1,0,1)
bhv_false <- c(1,0,1,0)
ctr <- c(1,1,0,0)
stz <- c(0,0,1,1)
ctr_bhv <- (as.numeric(bhv_false & ctr) - as.numeric(bhv_true & ctr))
stz_bhv <- (as.numeric(bhv_false & stz) - as.numeric(bhv_true & stz))
contrast(model.bhv_emm.raw, method = list(
  "CTR" = ctr_bhv,
  "STZ" = stz_bhv
))

df_binned_test_sess <- df_binned_test %>% filter(session_part != "overall")
# df_binned_test_sess <- df_binned_test_sess %>% filter(animal != "crni3")
selected_model_sess <- glmmTMB(precue_correct ~
                                 poly(day,2)*as.factor(treatment)*as.factor(session_part) +
                                 as.factor(treatment)*as.factor(bhv_performed)+
                                 # (1 | animal/day/session_part),
                                 (0+day | animal)+
                                 (1 | session_part:day:animal),
                               data = df_binned_test_sess,
                               family = poisson(),
                               control = glmmTMBControl(parallel = parallel::detectCores()))
model_performance(selected_model_sess)
check_overdispersion(selected_model_sess)
check_zeroinflation(selected_model_sess)
check_collinearity(selected_model_sess)
check_autocorrelation(selected_model_sess)
sim.res <- simulateResiduals(selected_model_sess, n = 1000, plot = F)
plot(sim.res)

ggplot(data = df_binned_test_sess, aes(x = day, color = as.factor(session_part), group = session_part)) +
  stat_summary_bin(data = df_binned_test_sess,
                   aes(x = day, y = precue_correct, color = session_part),
                   fun=mean,
                   geom="point")+
  #breaks = c(0, seq(12,240,24))) +
  stat_summary_bin(data = df_binned_test_sess,
                   aes(x = day, y = precue_correct, color = session_part),
                   fun.data=mean_se,
                   geom="errorbar")+
  #breaks = c(0, seq(12,240,24))) +
  stat_summary_bin(aes(y=fitted(selected_model_sess), color = session_part),
                   #breaks = c(0, seq(12,240,24)),
                   fun=mean, geom="line") +
  theme_bw(base_size=10) +
  #scale_y_sqrt()+
  facet_wrap(~animal*session_part)

model.session_emm <- emmeans(selected_model_sess, ~session_part|day|treatment, at=list(day = unique(df_test$day)))
model.session_contrasts.within_group.raw <- contrast(model.session_emm, method="trt.vs.ctrl", by=c("treatment", "day"))
model.session_contrasts.within_group.raw
model.session_emm.overall <- emmeans(selected_model_sess, ~session_part|treatment)
model.session_contrasts.overall <- contrast(model.session_emm.overall, method="trt.vs.ctrl", infer = c(T,T),by=c("session_part"))
model.session_contrasts.overall

model.session_means <- (
  emmeans(
    selected_model_sess,
    ~treatment|session_part|day,
    # at = list(hour = seq(min(df_test$hour),
    #                      max(df_test$hour),
    #                      0.5)
    # ),
    at = list(day = unique(df_test$day)),
    type = "response",
    adjust = "none"
  ) %>%
    summary(by = NULL)
)

model.session_means$session_part <- as.factor(model.session_means$session_part)

ggplot(data = model.session_means,
       aes(x = day, y = rate, group = interaction(treatment, session_part))) +
  geom_ribbon(
    data = model.session_means,
    aes(y = rate, ymin = asymp.LCL, ymax = asymp.UCL, fill = session_part), alpha = 0.1
  ) +
  geom_line(aes(color = session_part))+
  geom_point(
    data = model.session_means,
    aes(y = rate, color = session_part, shape = session_part),
    size = 3
  ) +
  theme_minimal() +
  scale_x_continuous(
    breaks = unique(df_binned_test$day),
    labels = function(x) { d = x+1 }
  ) +
  ylab("Number of started trials") +
  xlab("Day") +
  scale_color_brewer(palette = "Set2") +
  scale_fill_brewer(palette = "Set2") +
  ggtitle("Model-estimated number of trials over time [test easy]")+
  facet_wrap(~treatment)
