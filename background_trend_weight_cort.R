# Analysis of background trends - weight and mass

# 1. Load in data -----
bg_df <- read.csv("weight_cort_cap_1.csv", header = TRUE)

str(bg_df)

bg_df$date <- as.character(bg_df$Date)
bg_df$june_day <- as.numeric(substr(bg_df$date , 1, 2))


# packages ----
library(ggplot2)
library(cowplot)

library(lme4)
library(arm)
library(lattice)
library(MuMIn)

theme_new <- theme_bw(base_size = 14, base_family = "serif") +
  theme(legend.position = c(1, 1),
        legend.justification = c(1, 1),
        legend.key.size =   unit(2, "lines"),
        legend.key = element_rect(colour =NA),
        legend.text = element_text(size = 12),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        legend.text.align = 0,
        legend.key.width = unit(3, "lines"),
        legend.title = element_blank()
  )



# 2. Change in mass over time -----



# LMM
bg_df$june_day
mod.mass <- lm(Mass  ~june_day,
                          data = bg_df)

summary(mod.mass)
plot(mod.mass)


# Scatter plot + trend line
# Make ggplot figure, and add regression line
ggplot(data = bg_df, aes(y = Mass, x = june_day)) +
  geom_point() +
  geom_abline(intercept = 973.791, slope = -3.829, alpha = 0.5,lwd = 1) +
  theme_new +
  labs(x = "Day of June", y = "Mass (g)")
ggsave(filename = "change_mass_date.svg", width = 4, height = 4,
       units = "in")
ggsave(filename = "change_mass_date.png", width = 4, height = 4,
       units = "in")
ggsave(filename = "change_mass_date.pdf", width = 4, height = 4,
       units = "in")

# 3. Change in cort over time ----

bg_df$CORT
bg_df$log_cort <- log10(bg_df$CORT)
mod.cort <- lm(log_cort  ~june_day,
               data = bg_df)

summary(mod.cort)
plot(mod.cort)


# Scatter plot + trend line
# Make ggplot figure, and add regression line
ggplot(data = bg_df, aes(y = log_cort, x = june_day)) +
  geom_point() +
  geom_abline(intercept = 973.791, slope = -3.829, alpha = 0.5,lwd = 1) +
  theme_new +
  labs(x = "Day of June", y = expression("CORT" ~ (log[10])))
# ?paste
ggsave(filename = "change_logcort_date.svg", width = 4, height = 4,
       units = "in")
ggsave(filename = "change_logcort_date.png", width = 4, height = 4,
       units = "in")
ggsave(filename = "change_logcort_date.pdf", width = 4, height = 4,
       units = "in")

ggplot(data = bg_df, aes(y = CORT, x = june_day)) +
  geom_point() +
  geom_abline(intercept = 973.791, slope = -3.829, alpha = 0.5,lwd = 1) +
  theme_new +
  labs(x = "Day of June", y = "CORT")
# ?paste
ggsave(filename = "change_cort_date.svg", width = 4, height = 4,
       units = "in")
ggsave(filename = "change_cort_date.png", width = 4, height = 4,
       units = "in")
ggsave(filename = "change_cort_date.pdf", width = 4, height = 4,
       units = "in")



ggplot(data = bg_df, aes(y = CORT, x = june_day)) +
  geom_point() +
  geom_abline(intercept = 973.791, slope = -3.829, alpha = 0.5,lwd = 1) +
  theme_new +
  labs(x = "Day of June", y = "CORT")
