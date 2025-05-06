library(ggplot2)
library(dplyr)
library(readxl)
# 
# df <- read_excel("lmm_test1_diff_id.xlsx", sheet = "Sheet3")
# 
# df$subID <- as.factor(df$subID)
# 
# ggplot(df, aes(x = Groups, y = OpticalProperty, fill = Groups)) +
#   geom_boxplot() +
#   theme_minimal() +
#   labs(title = "Orientation by Group", x = "Group", y = "Orientation")
# 
# ggplot(df, aes(x = Groups, y = OpticalProperty, fill = Groups)) +
#   geom_violin(trim = FALSE, alpha = 0.5) +
#   theme_minimal() +
#   labs(title = "Orientation Density by Group", x = "Group", y = "Orientation")
# 
# ggplot(df, aes(x = OpticalProperty, fill = Groups)) +
#   geom_density(alpha = 0.5) +
#   theme_minimal() +
#   labs(title = "Orientation Distribution by Group", x = "Orientation", y = "Density")
# 
# ggplot(df, aes(x = subID, y = OpticalProperty, group = subID, color = Groups)) +
#   geom_line(alpha = 0.5) +
#   geom_point(size = 0.7) +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
#   labs(title = "Orientation Trends per Subject", x = "Subject ID", y = "Orientation")
# 
# ggplot(df, aes(x = Groups, y = OpticalProperty, fill = Groups)) +
#   geom_boxplot() +
#   facet_wrap(~ subID, scales = "free_y") +
#   theme_minimal() +
#   labs(title = "Orientation by Group for Each Subject", x = "Group", y = "Orientation")
# 

dfsame <- read_excel("Desktop/Consulting Optical/lmm_test1_same_id.xlsx", sheet = "orientation")


dfsame$subID <- as.factor(dfsame$subID)

ggplot(dfsame, aes(x = Groups, y = OpticalProperty, fill = Groups)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Orientation by Group", x = "Group", y = "Orientation")

ggplot(dfsame, aes(x = Groups, y = OpticalProperty, fill = Groups)) +
  geom_violin(trim = FALSE, alpha = 0.5) +
  theme_minimal() +
  labs(title = "Orientation Density by Group", x = "Group", y = "Orientation")

ggplot(dfsame, aes(x = OpticalProperty, fill = Groups)) +
  geom_density(alpha = 0.5) +
  theme_minimal() +
  labs(title = "Orientation Distribution by Group", x = "Orientation", y = "Density")

ggplot(dfsame, aes(x = Groups, y = OpticalProperty, fill = Groups)) +
  geom_boxplot() +
  facet_wrap(~ subID, scales = "free_y") +
  theme_minimal() +
  labs(title = "Orientation by Group for Each Subject", x = "Group", y = "Orientation")



ggplot(dfsame,aes(x=subID,y=OpticalProperty,fill=Groups))+
  geom_boxplot()+
  theme_minimal()+
  labs(title = "Orientation by Subject and Group", x = "Subject ID", y = "Orientation")

df_summary <- dfsame %>%
  group_by(subID, Groups) %>%
  summarise(mean = mean(OpticalProperty, na.rm = TRUE)) %>%
  ungroup()

ggplot(df_summary, aes(x = subID, y = mean, fill = Groups)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Mean Orientation by Subject ID and Group",
       x = "Subject ID", y = "Mean Orientation") +
  theme_minimal()




dfsame <- read_excel("lmm_test2_same_id.xlsx", sheet = "orientation")


dfsame$subID <- as.factor(dfsame$subID)

ggplot(dfsame, aes(x = Groups, y = OpticalProperty, fill = Groups)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Orientation by Group", x = "Group", y = "Orientation")

ggplot(dfsame, aes(x = Groups, y = OpticalProperty, fill = Groups)) +
  geom_violin(trim = FALSE, alpha = 0.5) +
  theme_minimal() +
  labs(title = "Orientation Density by Group", x = "Group", y = "Orientation")

ggplot(dfsame, aes(x = OpticalProperty, fill = Groups)) +
  geom_density(alpha = 0.5) +
  theme_minimal() +
  labs(title = "Orientation Distribution by Group", x = "Orientation", y = "Density")

ggplot(dfsame, aes(x = Groups, y = OpticalProperty, fill = Groups)) +
  geom_boxplot() +
  facet_wrap(~ subID, scales = "free_y") +
  theme_minimal() +
  labs(title = "Orientation by Group for Each Subject", x = "Group", y = "Orientation")



ggplot(dfsame,aes(x=subID,y=OpticalProperty,fill=Groups))+
  geom_boxplot()+
  theme_minimal()+
  labs(title = "Orientation by Subject and Group", x = "Subject ID", y = "Orientation")

df_summary <- dfsame %>%
  group_by(subID, Groups) %>%
  summarise(mean = mean(OpticalProperty, na.rm = TRUE)) %>%
  ungroup()

ggplot(df_summary, aes(x = subID, y = mean, fill = Groups)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Mean Orientation by Subject ID and Group",
       x = "Subject ID", y = "Mean Orientation") +
  theme_minimal()




dfsame <- read_excel("lmm_test3_same_id.xlsx", sheet = "orientation")


dfsame$subID <- as.factor(dfsame$subID)

ggplot(dfsame, aes(x = Groups, y = OpticalProperty, fill = Groups)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Orientation by Group", x = "Group", y = "Orientation")

ggplot(dfsame, aes(x = Groups, y = OpticalProperty, fill = Groups)) +
  geom_violin(trim = FALSE, alpha = 0.5) +
  theme_minimal() +
  labs(title = "Orientation Density by Group", x = "Group", y = "Orientation")

ggplot(dfsame, aes(x = OpticalProperty, fill = Groups)) +
  geom_density(alpha = 0.5) +
  theme_minimal() +
  labs(title = "Orientation Distribution by Group", x = "Orientation", y = "Density")

ggplot(dfsame, aes(x = Groups, y = OpticalProperty, fill = Groups)) +
  geom_boxplot() +
  facet_wrap(~ subID, scales = "free_y") +
  theme_minimal() +
  labs(title = "Orientation by Group for Each Subject", x = "Group", y = "Orientation")



ggplot(dfsame,aes(x=subID,y=OpticalProperty,fill=Groups))+
  geom_boxplot()+
  theme_minimal()+
  labs(title = "Orientation by Subject and Group", x = "Subject ID", y = "Orientation")

df_summary <- dfsame %>%
  group_by(subID, Groups) %>%
  summarise(mean = mean(OpticalProperty, na.rm = TRUE)) %>%
  ungroup()

ggplot(df_summary, aes(x = subID, y = mean, fill = Groups)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Mean Orientation by Subject ID and Group",
       x = "Subject ID", y = "Mean Orientation") +
  theme_minimal()

