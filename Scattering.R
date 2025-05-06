library(ggplot2)
library(dplyr)
library(readxl)
# 
# df <- read_excel("lmm_test1_diff_id.xlsx", sheet = "Sheet1")
# 
# df$subID <- as.factor(df$subID)
# 
# ggplot(df, aes(x = Groups, y = OpticalProperty, fill = Groups)) +
#   geom_boxplot() +
#   theme_minimal() +
#   labs(title = "Scattering by Group", x = "Group", y = "Scattering")
# 
# ggplot(df, aes(x = Groups, y = OpticalProperty, fill = Groups)) +
#   geom_violin(trim = FALSE, alpha = 0.5) +
#   theme_minimal() +
#   labs(title = "Scattering Density by Group", x = "Group", y = "Scattering")
# 
# ggplot(df, aes(x = OpticalProperty, fill = Groups)) +
#   geom_density(alpha = 0.5) +
#   theme_minimal() +
#   labs(title = "Scattering Distribution by Group", x = "Scattering", y = "Density")
# 
# 
# ggplot(df, aes(x = subID, y = OpticalProperty, group = subID, color = Groups)) +
#   geom_line(alpha = 0.5) +
#   geom_point(size = 0.7) +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
#   labs(title = "Scattering Trends per Subject", x = "Subject ID", y = "Scattering")
# 
# ggplot(df, aes(x = Groups, y = OpticalProperty, fill = Groups)) +
#   geom_boxplot() +
#   facet_wrap(~ subID, scales = "free_y") +
#   theme_minimal() +
#   labs(title = "Scattering by Group for Each Subject", x = "Group", y = "Scattering")
# 

dfsame <- read_excel("lmm_test1_same_id.xlsx", sheet = "scattering")


dfsame$subID <- as.factor(dfsame$subID)

df_sub4 <- dfsame[dfsame$subID == 4, ]

mean_OP <- mean(df_sub4$OpticalProperty, na.rm = TRUE)
sd_OP <- sd(df_sub4$OpticalProperty, na.rm = TRUE)

ggplot(df_sub4, aes(x = seq_along(OpticalProperty), y = OpticalProperty)) +
  geom_point(alpha = 0.5, color = "gray40") +
  geom_hline(yintercept = mean_OP, color = "blue", linetype = "solid", linewidth = 1) +
  geom_hline(yintercept = mean_OP + sd_OP, color = "darkgreen", linetype = "solid") +
  geom_hline(yintercept = mean_OP + 2*sd_OP, color = "orange", linetype = "solid") +
  geom_hline(yintercept = mean_OP + 3*sd_OP, color = "red", linetype = "solid") +
  labs(title = "Optical Property Values for Subject 4 with SD Lines",
       x = "Index (Measurement #)", y = "Optical Property") +
  theme_minimal()

df_sub4 <- df_sub4 %>%
  filter(OpticalProperty<(75))


df_other <- dfsame[dfsame$subID != 4, ]

dfsame <- rbind(df_other, df_sub4)


ggplot(dfsame, aes(x = Groups, y = OpticalProperty, fill = Groups)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Scattering by Group", x = "Group", y = "Scattering")

ggplot(dfsame, aes(x = Groups, y = OpticalProperty, fill = Groups)) +
  geom_violin(trim = FALSE, alpha = 0.5) +
  theme_minimal() +
  labs(title = "Scattering Density by Group", x = "Group", y = "Scattering")

ggplot(dfsame, aes(x = OpticalProperty, fill = Groups)) +
  geom_density(alpha = 0.5) +
  theme_minimal() +
  labs(title = "Scattering Distribution by Group", x = "Scattering", y = "Density")


ggplot(dfsame, aes(x = Groups, y = OpticalProperty, fill = Groups)) +
  geom_boxplot() +
  facet_wrap(~ subID, scales = "free_y") +
  theme_minimal() +
  labs(title = "Scattering by Group for Each Subject", x = "Group", y = "Scattering")

ggplot(dfsame,aes(x=subID,y=OpticalProperty,fill=Groups))+
  geom_boxplot()+
  theme_minimal()+
  labs(title = "Scattering by Subject and Group", x = "Subject ID", y = "Scattering")

df_summary <- dfsame %>%
  group_by(subID, Groups) %>%
  summarise(mean = mean(OpticalProperty, na.rm = TRUE)) %>%
  ungroup()

ggplot(df_summary, aes(x = subID, y = mean, fill = Groups)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Mean Scattering by Subject ID and Group",
       x = "Subject ID", y = "Mean Scattering") +
  theme_minimal()


dfsame <- read_excel("lmm_test2_same_id.xlsx", sheet = "scattering")


ggplot(dfsame, aes(x = Groups, y = OpticalProperty, fill = Groups)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Scattering by Group", x = "Group", y = "Scattering")

ggplot(dfsame, aes(x = Groups, y = OpticalProperty, fill = Groups)) +
  geom_violin(trim = FALSE, alpha = 0.5) +
  theme_minimal() +
  labs(title = "Scattering Density by Group", x = "Group", y = "Scattering")

ggplot(dfsame, aes(x = OpticalProperty, fill = Groups)) +
  geom_density(alpha = 0.5) +
  theme_minimal() +
  labs(title = "Scattering Distribution by Group", x = "Scattering", y = "Density")


ggplot(dfsame, aes(x = Groups, y = OpticalProperty, fill = Groups)) +
  geom_boxplot() +
  facet_wrap(~ subID, scales = "free_y") +
  theme_minimal() +
  labs(title = "Scattering by Group for Each Subject", x = "Group", y = "Scattering")

ggplot(dfsame,aes(x=subID,y=OpticalProperty,fill=Groups))+
  geom_boxplot()+
  theme_minimal()+
  labs(title = "Scattering by Subject and Group", x = "Subject ID", y = "Scattering")

df_summary <- dfsame %>%
  group_by(subID, Groups) %>%
  summarise(mean = mean(OpticalProperty, na.rm = TRUE)) %>%
  ungroup()

ggplot(df_summary, aes(x = subID, y = mean, fill = Groups)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Mean Scattering by Subject ID and Group",
       x = "Subject ID", y = "Mean Scattering") +
  theme_minimal()


dfsame <- read_excel("lmm_test3_same_id.xlsx", sheet = "scattering")


ggplot(dfsame, aes(x = Groups, y = OpticalProperty, fill = Groups)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Scattering by Group", x = "Group", y = "Scattering")

ggplot(dfsame, aes(x = Groups, y = OpticalProperty, fill = Groups)) +
  geom_violin(trim = FALSE, alpha = 0.5) +
  theme_minimal() +
  labs(title = "Scattering Density by Group", x = "Group", y = "Scattering")

ggplot(dfsame, aes(x = OpticalProperty, fill = Groups)) +
  geom_density(alpha = 0.5) +
  theme_minimal() +
  labs(title = "Scattering Distribution by Group", x = "Scattering", y = "Density")


ggplot(dfsame, aes(x = Groups, y = OpticalProperty, fill = Groups)) +
  geom_boxplot() +
  facet_wrap(~ subID, scales = "free_y") +
  theme_minimal() +
  labs(title = "Scattering by Group for Each Subject", x = "Group", y = "Scattering")

ggplot(dfsame,aes(x=subID,y=OpticalProperty,fill=Groups))+
  geom_boxplot()+
  theme_minimal()+
  labs(title = "Scattering by Subject and Group", x = "Subject ID", y = "Scattering")

df_summary <- dfsame %>%
  group_by(subID, Groups) %>%
  summarise(mean = mean(OpticalProperty, na.rm = TRUE)) %>%
  ungroup()

ggplot(df_summary, aes(x = subID, y = mean, fill = Groups)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Mean Scattering by Subject ID and Group",
       x = "Subject ID", y = "Mean Scattering") +
  theme_minimal()

