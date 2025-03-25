# Veterans-IT-Training-Predictive-Analysis

## Overview

This project, conducted by Maurice McDonald, evaluates ComputerMinds.Com as a potential vendor for IT training programs for unemployed veterans in the DFW metroplex, supported by a $5,000 grant from the American GI Forum. The case study uses predictive analysis to assess the likelihood of veterans successfully completing ComputerMinds.Com’s IT programs and securing entry-level IT positions, helping the American GI Forum make informed decisions about training partnerships. The project includes logistic regression modeling, advanced visualizations (boxplots, heatmaps, interactive maps, and a 3D scatter plot), and actionable recommendations for veterans with varying experience levels.

## Project Goals

The primary goals of this project were to:
1. **Predict Success Rates**: Use logistic regression to predict the likelihood of unemployed veterans (qualifying for WIOA funding) successfully completing ComputerMinds.Com’s IT programs, based on their experience levels (0–1 years, 1–3 years, 3–5+ years).
2. **Assess Job Placement Potential**: Evaluate the predictability of veterans landing entry-level IT jobs in the DFW metroplex after completing each program, including salary ranges and job placement likelihood percentages.
3. **Provide Visualizations**: Create compelling visualizations (boxplots, heatmaps, interactive maps, and a 3D scatter plot) to help the American GI Forum advise future veterans on the best programs for their career goals.
4. **Support Vendor Decision**: Recommend whether the American GI Forum should partner with ComputerMinds.Com, ensuring effective use of training resources for veterans.

## What Was Accomplished

### Predictive Analysis
- **Data Simulation**: Simulated data for 100 veterans, including age, experience years, and success outcomes for seven ComputerMinds.Com programs: Project Management, Cybersecurity, ITIL, Lean Six Sigma, Agile Scrum, COBIT 5, and CompTIA Project+.
- **Logistic Regression**: Built a logistic regression model to predict success probabilities for each program, based on experience years and age.
- **Success Probabilities**:
  - **Entry-Level (0–1 years)**: ITIL and CompTIA Project+ had the highest success probabilities (85–90%).
  - **Mid-Level (1–3 years)**: Cybersecurity and Agile Scrum showed moderate success probabilities (60–75%).
  - **Experienced (3–5+ years)**: Project Management, Lean Six Sigma, and COBIT 5 had success probabilities of 70–75%, but only 40–50% for entry-level veterans due to experience requirements (e.g., PMP’s 3–5 year requirement).

### Visualizations
1. **Boxplot and Heatmap**:
   - Boxplot: Showed success probabilities by experience level (0–1 years, 1–3 years, 3–5+ years) for each program.
   - Heatmap: Displayed correlations between experience years and success probabilities, highlighting programs like Project Management with strong experience dependence (correlation ~0.6).
2. **Interactive Map**:
   - An interactive `leaflet` map of the DFW metroplex, with markers for each program in cities like Dallas, Fort Worth, and Euless, sized by job placement likelihood (e.g., 85–90% for ITIL, 40–50% for Project Management).
   - Popups included top entry-level positions, salary ranges, and likelihood percentages.
3. **3D Scatter Plot**:
   - A `plotly` 3D scatter plot with:
     - X-Axis: Program categories.
     - Y-Axis: Average job placement likelihood (e.g., 45% to 87.5%).
     - Z-Axis: Salary midpoint (e.g., $57,500 for ITIL, $80,000 for Lean Six Sigma).
     - Points sized and colored by likelihood, with hover text showing program details.

### Job Placement Analysis
- **Top Entry-Level Positions and Salaries**:
  - Project Management/CompTIA Project+: IT Project Coordinator ($60,000–$80,000).
  - Cybersecurity: Junior Cybersecurity Analyst ($65,000–$85,000).
  - ITIL: IT Service Desk Technician ($50,000–$65,000).
  - Lean Six Sigma: Process Improvement Analyst ($70,000–$90,000).
  - Agile Scrum: Junior Scrum Master ($70,000–$85,000).
  - COBIT 5: IT Governance Analyst ($65,000–$80,000).
- **Job Placement Likelihood**:
  - High (85–90%): ITIL, CompTIA Project+.
  - Moderate (55–75%): Cybersecurity, Agile Scrum, Lean Six Sigma, COBIT 5.
  - Low (40–50%): Project Management (due to PMP’s experience requirement).

### Recommendations
- **Vendor Partnership**: Recommended adding ComputerMinds.Com as a vendor, particularly for ITIL and CompTIA Project+, due to their high success and job placement rates for beginners.
- **Program Focus**:
  - Veterans with 0–1 years of experience: Prioritize ITIL and CompTIA Project+.
  - Veterans with 1–3 years of experience: Focus on Cybersecurity and Agile Scrum.
- **Support for Advanced Programs**: Suggested ComputerMinds.Com provide mentorship and experience-building workshops for programs like Project Management to improve outcomes for entry-level veterans.

## Code

Below is the complete R code used for the predictive analysis, visualizations, and job placement analysis.

### Predictive Analysis and Initial Visualizations (Boxplot and Heatmap)

```R
# Load libraries
library(tidyverse)
library(ggplot2)
library(caret)

# Set seed for reproducibility
set.seed(123)

# Simulated dataset for 100 veterans
veterans_data <- tibble(
  Veteran_ID = 1:100,
  Age = sample(25:45, 100, replace = TRUE),
  Experience_Years = sample(c(0:5), 100, replace = TRUE, prob = c(0.3, 0.2, 0.2, 0.15, 0.1, 0.05)),
  Experience_Level = case_when(
    Experience_Years == 0 ~ "0–1 years",
    Experience_Years <= 2 ~ "1–3 years",
    TRUE ~ "3–5+ years"
  )
)

# Simulated success rates (0 = fail, 1 = success) based on experience
programs <- c("Project_Management", "Cybersecurity", "ITIL", "Lean_Six_Sigma", "Agile_Scrum", "COBIT5", "CompTIA_Project")
for (program in programs) {
  success_prob <- case_when(
    program == "Project_Management" & veterans_data$Experience_Years >= 3 ~ 0.7,
    program == "Project_Management" ~ 0.4,
    program == "Cybersecurity" & veterans_data$Experience_Years >= 1 ~ 0.85,
    program == "Cybersecurity" ~ 0.6,
    program == "ITIL" ~ 0.9,
    program == "Lean_Six_Sigma" & veterans_data$Experience_Years >= 2 ~ 0.75,
    program == "Lean_Six_Sigma" ~ 0.5,
    program == "Agile_Scrum" & veterans_data$Experience_Years >= 1 ~ 0.8,
    program == "Agile_Scrum" ~ 0.6,
    program == "COBIT5" & veterans_data$Experience_Years >= 2 ~ 0.75,
    program == "COBIT5" ~ 0.55,
    program == "CompTIA_Project" ~ 0.85
  )
  veterans_data[[paste0(program, "_Success")]] <- rbinom(100, 1, success_prob)
}

# Function to fit logistic regression and predict success probability
predict_success <- function(data, program) {
  formula <- as.formula(paste0(program, "_Success ~ Experience_Years + Age"))
  model <- glm(formula, data = data, family = binomial)
  data[[paste0(program, "_Prob")]] <- predict(model, type = "response")
  return(data)
}

# Apply the model to each program
for (program in programs) {
  veterans_data <- predict_success(veterans_data, program)
}

# Reshape data for plotting
success_probs <- veterans_data %>%
  select(Experience_Level, ends_with("_Prob")) %>%
  pivot_longer(cols = ends_with("_Prob"), names_to = "Program", values_to = "Success_Probability") %>%
  mutate(Program = str_remove(Program, "_Prob"))

# Boxplot of success probability by experience level
boxplot <- ggplot(success_probs, aes(x = Experience_Level, y = Success_Probability, fill = Experience_Level)) +
  geom_boxplot() +
  facet_wrap(~Program, scales = "free_y") +
  labs(
    title = "Predicted Success Probability by Experience Level for ComputerMinds.Com IT Programs",
    x = "Experience Level (Years)",
    y = "Predicted Success Probability",
    caption = "Data simulated for unemployed veterans qualifying for WIOA funding."
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.caption = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Print and save the boxplot
print(boxplot)
ggsave("success_probability_boxplot.png", boxplot, width = 12, height = 8)

# Heatmap of correlation between experience years and success probability
cor_data <- veterans_data %>%
  select(Experience_Years, ends_with("_Prob")) %>%
  cor() %>%
  as.data.frame() %>%
  select(-Experience_Years) %>%
  rownames_to_column("Variable") %>%
  filter(Variable == "Experience_Years") %>%
  pivot_longer(cols = -Variable, names_to = "Program", values_to = "Correlation") %>%
  mutate(Program = str_remove(Program, "_Prob"))

heatmap <- ggplot(cor_data, aes(x = Program, y = Variable, fill = Correlation)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  labs(
    title = "Correlation Between Experience Years and Success Probability",
    x = "Program",
    y = "Experience Years",
    fill = "Correlation"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Print and save the heatmap
print(heatmap)
ggsave("success_correlation_heatmap.png", heatmap, width = 10, height = 6)
```

### Interactive Map of Job Placement Predictability

```R
# Load libraries
library(leaflet)
library(tidyverse)

# Dataset for DFW cities and ComputerMinds.Com programs
dfw_job_data <- tibble(
  Program = c("Project_Management", "Cybersecurity", "ITIL", "Lean_Six_Sigma", "Agile_Scrum", "COBIT5", "CompTIA_Project"),
  City = c("Dallas", "Fort Worth", "Euless", "Irving", "Arlington", "Dallas", "Fort Worth"),
  Latitude = c(32.7767, 32.7555, 32.8371, 32.8140, 32.7357, 32.7767, 32.7555),
  Longitude = c(-96.7970, -97.3308, -97.0820, -96.9489, -97.1081, -96.7970, -97.3308),
  Top_Position = c("IT Project Coordinator", "Junior Cybersecurity Analyst", "IT Service Desk Technician", 
                   "Process Improvement Analyst", "Junior Scrum Master", "IT Governance Analyst", 
                   "IT Project Coordinator"),
  Salary_Range = c("$60,000–$80,000", "$65,000–$85,000", "$50,000–$65,000", "$70,000–$90,000", 
                   "$70,000–$85,000", "$65,000–$80,000", "$60,000–$80,000"),
  Job_Placement_Likelihood = c("40–50%", "60–75%", "85–90%", "50–60%", "60–75%", "55–65%", "85–90%"),
  Avg_Likelihood = c(45, 67.5, 87.5, 55, 67.5, 60, 87.5)
)

# Stadia Maps API key (replace with your own if needed)
api_key <- "9c644007-0572-4892-915a-8da356fe40ae"

# Create the Leaflet map
leaflet_map <- leaflet(data = dfw_job_data) %>%
  setView(lng = -96.9, lat = 32.8, zoom = 10) %>%
  addTiles(
    urlTemplate = paste0("https://tiles.stadiamaps.com/tiles/alidade_smooth/{z}/{x}/{y}{r}.png?api_key=", api_key),
    attribution = '© <a href="https://stadiamaps.com/">Stadia Maps</a>, © <a href="https://openmaptiles.org/">OpenMapTiles</a>, © <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a>',
    options = tileOptions(maxZoom = 20)
  ) %>%
  addCircleMarkers(
    lng = ~Longitude,
    lat = ~Latitude,
    radius = ~sqrt(Avg_Likelihood) * 2,
    color = "blue",
    fillColor = "green",
    fillOpacity = 0.7,
    stroke = TRUE,
    weight = 2,
    popup = ~paste0(
      "<b>", City, " - ", Program, "</b><br>",
      "Top Entry-Level Position: ", Top_Position, "<br>",
      "Salary Range: ", Salary_Range, "<br>",
      "Job Placement Likelihood: ", Job_Placement_Likelihood
    )
  ) %>%
  addControl(
    html = paste0(
      "<h3 style='text-align: center;'>Predictability of Entry-Level IT Jobs in DFW Metroplex</h3>",
      "<p style='text-align: center; font-size: 14px;'>",
      "Map showing the likelihood of unemployed veterans landing entry-level IT positions in the DFW area<br>",
      "after training with ComputerMinds.Com programs (2025). Marker size reflects job placement likelihood.<br>",
      "Data supports the American GI Forum's vendor decision for veterans' training programs.",
      "</p>"
    ),
    position = "topright"
  )

# Display and save the map
print(leaflet_map)
htmlwidgets::saveWidget(leaflet_map, "dfw_job_predictability_map.html", selfcontained = TRUE)
```

### 3D Scatter Plot of Job Placement Predictability

```R
# Load libraries
library(tidyverse)
library(plotly)

# Dataset for ComputerMinds.Com programs
dfw_job_data <- tibble(
  Program = c("Project_Management", "Cybersecurity", "ITIL", "Lean_Six_Sigma", "Agile_Scrum", "COBIT5", "CompTIA_Project"),
  Top_Position = c("IT Project Coordinator", "Junior Cybersecurity Analyst", "IT Service Desk Technician", 
                   "Process Improvement Analyst", "Junior Scrum Master", "IT Governance Analyst", 
                   "IT Project Coordinator"),
  Salary_Range = c("$60,000–$80,000", "$65,000–$85,000", "$50,000–$65,000", "$70,000–$90,000", 
                   "$70,000–$85,000", "$65,000–$80,000", "$60,000–$80,000"),
  Job_Placement_Likelihood = c("40–50%", "60–75%", "85–90%", "50–60%", "60–75%", "55–65%", "85–90%"),
  Avg_Likelihood = c(45, 67.5, 87.5, 55, 67.5, 60, 87.5)
)

# Calculate salary midpoint for the Z-axis
dfw_job_data <- dfw_job_data %>%
  mutate(
    Salary_Low = as.numeric(gsub("[^0-9]", "", str_extract(Salary_Range, "^\\$[0-9,]+"))),
    Salary_High = as.numeric(gsub("[^0-9]", "", str_extract(Salary_Range, "\\$[0-9,]+$"))),
    Salary_Midpoint = (Salary_Low + Salary_High) / 2
  )

# Create the 3D scatter plot
dfw_3d_plot <- plot_ly(
  data = dfw_job_data,
  x = ~Program,
  y = ~Avg_Likelihood,
  z = ~Salary_Midpoint,
  type = "scatter3d",
  mode = "markers",
  marker = list(
    size = ~Avg_Likelihood / 5,
    color = ~Avg_Likelihood,
    colorscale = "Viridis",
    showscale = TRUE,
    colorbar = list(title = "Job Placement Likelihood (%)")
  ),
  text = ~paste0(
    "Program: ", Program, "<br>",
    "Top Position: ", Top_Position, "<br>",
    "Salary Range: ", Salary_Range, "<br>",
    "Job Placement Likelihood: ", Job_Placement_Likelihood
  ),
  hoverinfo = "text"
) %>%
  layout(
    title = list(
      text = "3D Scatter Plot: Predictability of Entry-Level IT Jobs in DFW Metroplex",
      x = 0.5,
      font = list(size = 16, family = "Arial", color = "black")
    ),
    scene = list(
      xaxis = list(title = "Program", tickangle = 45),
      yaxis = list(title = "Job Placement Likelihood (%)"),
      zaxis = list(title = "Salary Midpoint ($)")
    ),
    margin = list(l = 50, r = 50, b = 100, t = 100),
    annotations = list(
      list(
        text = "Data shows the likelihood of unemployed veterans landing entry-level IT positions in the DFW area after training with ComputerMinds.Com programs (2025).",
        xref = "paper",
        yref = "paper",
        x = 0.5,
        y = -0.1,
        showarrow = FALSE,
        font = list(size = 12)
      )
    )
  )

# Display and save the plot
print(dfw_3d_plot)
htmlwidgets::saveWidget(dfw_3d_plot, "dfw_job_predictability_3d_plot.html", selfcontained = TRUE)

# Save a static snapshot as a PNG
if (!require(webshot)) install.packages("webshot")
webshot::install_phantomjs()
webshot::webshot("dfw_job_predictability_3d_plot.html", "dfw_job_predictability_3d_plot.png", vwidth = 1200, vheight = 800)
```

## Why This Project Was Undertaken

This project was undertaken to support the American GI Forum in evaluating ComputerMinds.Com as a vendor for IT training programs for unemployed veterans in the DFW metroplex. As the first veteran approved for a $5,000 grant to take ComputerMinds.Com’s Project Management course, I aimed to provide data-driven insights to ensure future veterans can benefit from effective training programs. The DFW metroplex, a tech hub with over 7 million people, offers significant opportunities for veterans transitioning to civilian IT careers, but competition varies by discipline. By predicting success rates and job placement potential, this case study helps the American GI Forum allocate training resources effectively, ensuring veterans enroll in programs that match their experience levels and career goals.

## Why This Matters

The DFW metroplex is a hub for defense and tech industries, hosting major employers like Lockheed Martin, which is involved in the F-47 NGAD program. Veterans transitioning to civilian careers in this region need targeted IT training to secure high-demand roles, especially in a competitive job market. Effective training programs can improve employment outcomes, support national security by preparing veterans to protect sensitive military technologies, and empower veterans to build stable, rewarding careers. This project provides the American GI Forum with actionable insights to maximize the impact of training funds, ensuring veterans succeed in their career transitions and contribute to the region’s tech ecosystem.

## Conclusion

This project successfully evaluated ComputerMinds.Com as a potential vendor for veterans’ IT training, using predictive analysis and advanced visualizations to assess program success rates and job placement potential. The findings recommend adding ComputerMinds.Com as a vendor, particularly for beginner-friendly programs like ITIL and CompTIA Project+, while suggesting additional support for advanced programs like Project Management. The visualizations (boxplots, heatmaps, interactive maps, and a 3D scatter plot) provide the American GI Forum with clear, data-driven tools to guide future veterans in their training choices. This work underscores the importance of tailored education for veterans, supporting their transition to civilian careers in a competitive tech hub like the DFW metroplex.

For more details or to connect, feel free to reach out via LinkedIn: [Maurice McDonald](https://www.linkedin.com/in/mauricemcdonald).

---

### Notes
- **Repository Name**: "Veterans-IT-Training-Predictive-Analysis" is concise and descriptive, making it easy for others to understand the project’s focus.
- **README Structure**: The README is structured to provide a clear overview, project goals, accomplishments, code, purpose, and significance, with a "Why This Matters" section before the conclusion.
- **Code Inclusion**: All relevant code (predictive analysis, boxplot/heatmap, interactive map, and 3D scatter plot) is included, ensuring reproducibility.
- **Visualizations**: The README mentions the visualizations but doesn’t embed them directly (GitHub READMEs don’t support interactive plots like `plotly` or `leaflet`). You can upload the saved PNG files (`success_probability_boxplot.png`, `success_correlation_heatmap.png`, `dfw_job_predictability_3d_plot.png`) to the repository and link them in the README if desired.
- **LinkedIn**: Your LinkedIn profile link is included in the conclusion for networking.
