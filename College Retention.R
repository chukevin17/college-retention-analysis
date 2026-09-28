# College Retention
# Model & Predictor Significance (0.05)

# Load libraries
library(readxl)
library(Hmisc)
library(pscl)
library(pROC)

# H0: p = 0 (No linear relationship)
# Ha: p ≠ 0 (Linear relationship exists)

# Import Lakeland College data
college_df <- read_excel(file.choose())

# Clean data - drop student column
coll_df <- subset(college_df, select = -c(Student))

# Summarize data
head(coll_df)
summary(coll_df)

# Interpretation: The median GPA is 2.7, with a median of 1 meaning studnets attended the orientation and
# would likely return for the sophomore year.

# Correlation analysis
corr <- rcorr(as.matrix(coll_df))
corr

# Interpretation:
# GPA and Return (r = 0.58):Moderate to strong positive correlation. Students with higher GPAs are more likely to return for their sophomore year.
# Program and Return (r = 0.52):Moderate positive correlation. Participation in the orientation/support program is positively associated with student retention.
# GPA and Program (r = 0.50):Moderate positive correlation. Students participating in the program tend to have higher GPAs.
# Interpretation: Because all p-values are well below the standard threshold of alpha = 0.05, every correlation in this matrix is statistically significant.
# You can confidently reject the null hypothesis that these variables have no linear relationship in the population.

# Build logistic regression
model <- glm(Return ~ GPA + Program, data = coll_df, famil = binomial)
summary(model)

# Interpretation:
# GPA (p = 0.000161 < 0.001): Highly statistically significant. GPA is a strong predictor of student retention.
# Program (p = 0.005579 < 0.01): Statistically significant. Participating in the program significantly improves retention even after controlling for student GPA.

library(ggplot2)
library(patchwork)

# Custom color palette for institutional retention
c_green <- "#2E7D32"
c_red   <- "#C62828"
c_blue  <- "#1565C0"
c_gray  <- "#F5F5F5"

# Top Panel: KPI Cards
kpi_card <- function(title, value, subtitle, col) {
  ggplot() +
    annotate("rect", xmin = 0, xmax = 1, ymin = 0, ymax = 1, fill = c_gray, color = NA) +
    annotate("text", x = 0.5, y = 0.75, label = title, size = 3.5, fontface = "bold", color = "#555555") +
    annotate("text", x = 0.5, y = 0.45, label = value, size = 8, fontface = "bold", color = col) +
    annotate("text", x = 0.5, y = 0.20, label = subtitle, size = 3, color = "#777777") +
    theme_void()
}

k1 <- kpi_card("TOTAL ENROLLMENT", "100", "Students Tracked", "#333333")
k2 <- kpi_card("RETENTION RATE", "66.0%", "Overall Return Rate", c_green)
k3 <- kpi_card("MEDIAN GPA", "2.74", "Cohort Baseline", c_blue)

# Bottom Left: GPA Bin Retention Distribution
p_bins <- ggplot(coll_df, aes(x = cut(GPA, breaks = c(0, 2.0, 2.5, 3.0, 3.5, 4.0)), fill = factor(Return))) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c(c_red, c_green), labels = c("Left", "Returned")) +
  labs(title = "Retention Rate by GPA Tier", x = "GPA Bracket", y = "Proportion", fill = "Status") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Bottom Right: Logistic Sigmoid / Threshold Curve
p_prob <- ggplot(coll_df, aes(x = GPA, y = Return)) +
  geom_point(aes(color = factor(Return)), alpha = 0.5, size = 2) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), color = c_blue, linewidth = 1.2, se = FALSE) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray50") +
  scale_color_manual(values = c(c_red, c_green), guide = "none") +
  labs(title = "Predicted Return Probability vs. GPA", x = "Student GPA", y = "Probability of Returning") +
  theme_minimal()

# Assemble Layout: 1 Row of KPIs on top, 2 Charts on bottom
kpi_row <- (k1 | k2 | k3)
charts_row <- (p_bins | p_prob)

exec_dashboard <- kpi_row / charts_row + plot_layout(heights = c(1, 2.5)) +
  plot_annotation(title = "Executive Summary: Student Retention Analysis",
                  theme = theme(plot.title = element_text(size = 16, face = "bold")))

ggsave("executive_retention_dashboard.png", exec_dashboard, width = 11, height = 7, dpi = 300)

# Save the high-res file for your LinkedIn upload
ggsave("executive_retention_dashboard.png", exec_dashboard, width = 11, height = 7, dpi = 300)

# ADD THIS LINE to display it immediately in your Plots tab:
exec_dashboard
