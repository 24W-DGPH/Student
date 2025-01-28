#Visualization

# Prepare the Age Table

# Summarize data by primary diagnosis and age groups
table_age <- hosp_admi %>%
  group_by(primary_diagnosis) %>%  
  summarise(
    Age_0_9 = sum(Age_0_9, na.rm = TRUE),
    Age_10_19 = sum(Age_10_19, na.rm = TRUE),
    Age_20_29 = sum(Age_20_29, na.rm = TRUE),
    Age_30_39 = sum(Age_30_39, na.rm = TRUE),
    Age_40_49 = sum(Age_40_49, na.rm = TRUE),
    Age_50_59 = sum(Age_50_59, na.rm = TRUE),
    Age_60_69 = sum(Age_60_69, na.rm = TRUE),
    Age_70_79 = sum(Age_70_79, na.rm = TRUE),
    Age_80_90 = sum(Age_80_90, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(
    Total = Age_0_9 + Age_10_19 + Age_20_29 + Age_30_39 + Age_40_49 + 
      Age_50_59 + Age_60_69 + Age_70_79 + Age_80_90
  )

# Prepare the table for visualization
table_age_for_kable <- table_age %>%
  select(primary_diagnosis, Age_0_9, Age_10_19, Age_20_29, Age_30_39, Age_40_49, 
         Age_50_59, Age_60_69, Age_70_79, Age_80_90)

# Beautify the Age Table with kableExtra
table_age_for_kable %>%
  kable(
    format = "html",
    caption = "Age Distribution by Primary Diagnosis"
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed", "responsive"),
    full_width = FALSE
  ) %>%
  row_spec(0, bold = TRUE, color = "white", background = "#ADD8E6", extra_css = "border: 1px solid black;") %>%
  row_spec(1, extra_css = "border-top: 1px solid black;") %>%
  row_spec(nrow(table_age_for_kable), extra_css = "border-bottom: 1px solid black;") %>%
  column_spec(1, bold = TRUE, border_right = TRUE, border_left = TRUE) %>%
  column_spec(2:ncol(table_age_for_kable), border_right = TRUE, border_left = TRUE)

# Prepare the Gender Table
table_gender <- hosp_admi %>%
  group_by(primary_diagnosis) %>%  
  summarise(
    male = sum(male, na.rm = TRUE),
    female = sum(female, na.rm = TRUE),
    gender_unknown = sum(gender_unknown, na.rm = TRUE),
    .groups = 'drop'  
  ) %>%
  mutate(
    Total = male + female + gender_unknown
  )

# Prepare gender table for visualization
table_gender_for_kable <- table_gender %>%
  select(primary_diagnosis, male, female, gender_unknown)

# Beautify the Gender Table
table_gender_for_kable %>%
  kable(
    format = "html",
    caption = "Gender Distribution by Primary Diagnosis",
    align = "c"
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed", "responsive"),
    full_width = FALSE,
    position = "center"
  ) %>%
  row_spec(0, bold = TRUE, color = "white", background = "#ADD8E6", extra_css = "border: 1px solid black;") %>%
  row_spec(1, extra_css = "border-top: 1px solid black;") %>%
  row_spec(nrow(table_gender_for_kable), extra_css = "border-bottom: 1px solid black;") %>%
  column_spec(1, bold = TRUE, border_right = TRUE, border_left = TRUE) %>%
  column_spec(2:ncol(table_gender_for_kable), border_right = TRUE, border_left = TRUE)

# Prepare data for plotting
plot_data <- table_age %>%
  select(primary_diagnosis, starts_with("Age_")) %>%
  pivot_longer(
    cols = starts_with("Age_"), 
    names_to = "Age_Group", 
    values_to = "Count"
  )

# Create the age distribution bar chart
chart_bar_age <- ggplot(plot_data, aes(x = primary_diagnosis, y = Count, fill = Age_Group)) +
  geom_bar(stat = "identity", position = "stack", color = "black") +
  labs(
    title = "Age Distribution by Primary Diagnosis",
    x = "Primary Diagnosis",
    y = "Count",
    fill = "Age Group"
  ) +
  scale_y_continuous(labels = comma) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold")
  )

# Prepare gender data for plotting
plot_gender_data <- table_gender %>%
  pivot_longer(
    cols = c(male, female, gender_unknown),
    names_to = "Gender",
    values_to = "Count"
  )

# Create the gender distribution bar chart
chart_bar_gender <- ggplot(plot_gender_data, aes(x = primary_diagnosis, y = Count, fill = Gender)) +
  geom_bar(stat = "identity", position = "stack", color = "black") +
  labs(
    title = "Gender Distribution by Primary Diagnosis",
    x = "Primary Diagnosis",
    y = "Count",
    fill = "Gender"
  ) +
  scale_fill_manual(values = c("male" = "#ADD8E6", "female" = "#FFDAB9", "gender_unknown" = "#90EE90")) +
  scale_y_continuous(labels = comma) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold")
  )
