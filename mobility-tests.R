
# title: "Lesson 3 : Workflow and Statistical Test in R"

# author: "Nelson Semidey"

# Date: "2025-17-7"

# Packages

library(arrow)

library(tidyverse)


# Merge

merged_data <- read_parquet(file = "merged_data.parquet", stringsAsFactors = TRUE)


# Boxplot

boxplot(formula = upward_mobility_rate_2010 ~ STATE_NAME_2010SVI, data = merged_data)

merged_data <- merged_data[!(merged_data$STATE_NAME_2010SVI %in% "06"), ]


# Box plot 2

boxplot(formula = M_TOTPOP_2010SVI ~ STATE_NAME_2010SVI, data = merged_data)



# tidyverse and Advanced Data Handling

# Case sensitivity

boxplot(formula = M_TOTPOP_2010SVI ~ state_NAME_2010SVI, data = merged_data)


# tidyverse

subset_data_ca <- subset(merged_data, STATE_ABBR_2010SVI == "CA")

subset_data_az <- subset(merged_data, STATE_ABBR_2010SVI == "AZ")

print(upward_mean_2010_ca <- mean(subset_data_ca$upward_mobility_rate_2010))

print(upward_mean_2010_az <- mean(subset_data_az$upward_mobility_rate_2010))

upward_mobility_means_2010 <- data.frame(State = c("CA", "AZ"),
                                         up_2010_mean = c(upward_mean_2010_ca, upward_mean_2010_az))


# actual tidyverse

state_group <- merged_data %>%
  group_by(STATE_ABBR_2010SVI)

state_mob_means <- state_group %>%
  summarise(up_mean = mean(upward_mobility_rate_2010, na.rm = TRUE))

state_mob_means <- state_mob_means %>%
  filter(!is.na(STATE_ABBR_2010SVI))


# Pipe Operator

upward <- merged_data %>%
  group_by(STATE_ABBR_2010SVI) %>%
  summarise(means_2010 = mean(upward_mobility_rate_2010, na.rm = TRUE))

upward <- upward %>% filter(!is.na(STATE_ABBR_2010SVI))


# Error Bars with ggplot2

upward_stats <- merged_data %>%
  group_by(STATE_ABBR_2010SVI, COUNTY_2010SVI) %>%
  summarise(up_means = mean(upward_mobility_rate_2010, na.rm = TRUE),
            up_se = sd(upward_mobility_rate_2010, na.rm = TRUE)/sqrt(n()))
upward_stats <- upward_stats %>% filter(!is.na(STATE_ABBR_2010SVI))


# Error Bars

ggplot(data = upward_stats, mapping = aes(x = STATE_ABBR_2010SVI, y = up_means)) +
  geom_point()


ggplot(data = upward_stats, mapping = aes(x = STATE_ABBR_2010SVI, y = up_means)) +
  geom_point() +
  geom_errorbar(mapping = aes(ymin = up_means - up_se, ymax = up_means + up_se))


ggplot(data = upward_stats, mapping = aes(x = STATE_ABBR_2010SVI, y = up_means)) +
  geom_point() +
  geom_errorbar(mapping = aes(ymin = up_means - up_se, ymax = up_means + up_se), width = 0.30)


# Cleaned up by State 

upward_stats_st <- merged_data %>%
  group_by(STATE_ABBR_2010SVI) %>%
  summarise(up_means = mean(upward_mobility_rate_2010),
            up_se = sd(upward_mobility_rate_2010)/sqrt(n()))

upward_stats_st <- upward_stats_st %>% filter(!is.na(STATE_ABBR_2010SVI))


ggplot(data = upward_stats_st, mapping = aes(x = STATE_ABBR_2010SVI, y = up_means)) +
  geom_point() +
  geom_errorbar(mapping = aes(ymin = up_means - up_se, ymax = up_means + up_se), width = 0.30)


# more tidyverse tricks

merged <- merged_data

merged %>%
  dplyr:: select(!contains("_"), starts_with("upward"))


# reorder columns

merged <- merged %>%
  dplyr:: relocate(contains("STATE"), .after = upward_mobility_rate_2020)


# create a unique identifier

unique_id <- merged %>%
  dplyr:: group_by(STATE_FIPS_2010SVI, COUNTY_2010SVI, CENSUSAREA_2010SVI) %>%
  dplyr:: mutate(uniqueid = row_number(), .before = contains("_"))


# Summarize 

merged_stats_up <- merged %>%
  dplyr::group_by(STATE_FIPS_2010SVI) %>%
  dplyr::summarize(across(starts_with("upward"),
                          list(~mean(.x, na.rm = TRUE),
                               ~sd(.x, na.rm = TRUE))))

merged_stats_up <- merged_stats_up %>% filter(!is.na(STATE_FIPS_2010SVI))


# Rename Columns Summary

merged_stats_up <- merged %>%
  dplyr::group_by(STATE_ABBR_2010SVI) %>%
  dplyr::summarize(across(starts_with("upward"),
                          list(mean = ~mean(.x, na.rm = TRUE)),
                               .names = "{gsub('_', '', col)}_{fn}"))

merged_stats_up <- merged_stats_up %>% filter(!is.na(STATE_ABBR_2010SVI))


# Modeling and Nesting
# Does not work

upward_models <- merged %>%
  group_by(STATE_ABBR_2010SVI) %>% summarise(model.frame(lm(upward_mobility_rate_2010 ~ POP2010)))

merged <- nest_by(state_group)


# Running a Basic Stat Test

merged <- merged_data

az <- merged_data[merged_data$STATE_NAME_2010SVI == "Arizona", ]

ca <- merged_data[merged_data$STATE_NAME_2010SVI == "California", ]

dim(merged_data)


# SEE column

merged_data[3,]

merged_data[,3]

print(nrow(az))

print(nrow(ca))

t.test(x = az$upward_mobility_rate_2010, y = ca$upward_mobility_rate_2010)


# ANOVA

aov(formula = upward_mobility_rate_2010 ~ COUNTY_2010SVI, data = az)


mobility_rate_az_aov <- aov(formula = upward_mobility_rate_2010 ~ COUNTY_2010SVI, data = az)


summary(object = mobility_rate_az_aov)

sink(file = "output/az_mobility_anova.txt")

summary(object = mobility_rate_az_aov)

sink()


# Linear Regression

summary(merged_data[,1:10], 6)

plot(x = merged_data$upward_mobility_rate_2010, y = merged_data$M_TOTPOP_2010SVI)

merged$logpop <- log10(merged$M_TOTPOP_2010SVI)

plot(x = merged$upward_mobility_rate_2010,
     y = merged$logpop,
     xlab = "Upward Mobility",
     ylab = "log10(population)")


mobility_v_pop <- lm(upward_mobility_rate_2010 ~ logpop, data = merged)

summary(mobility_v_pop)


sink(file = "output/mobility-pop-regression.txt")

summary(mobility_v_pop)

sink()


# Multivariate Regression

merged$az <- ifelse(merged$STATE_NAME_2010SVI == "Arizona", 1, 0)

merged$ca <-ifelse(merged$STATE_NAME_2010SVI == "California", 1, 0)


mobility_v_pop_state <- lm(formula = upward_mobility_rate_2010 ~ logpop + az, data = merged)

summary(mobility_v_pop_state)
