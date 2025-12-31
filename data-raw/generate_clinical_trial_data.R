## Code to generate the clinical_trial_data dataset

# This script generates a comprehensive synthetic clinical trial dataset
# with 100 subjects and 41 variables for testing all descriptR functions

set.seed(12345)  # For reproducibility

n <- 100  # Number of subjects

# Helper function for generating correlated variables
generate_correlated <- function(base, r, sd_new = 1) {
  noise <- rnorm(length(base), 0, sqrt(1 - r^2))
  scale(base) * r + noise * sqrt(1 - r^2) * sd_new
}

# Generate clinical trial data
clinical_trial_data <- data.frame(
  # ===== IDENTIFIERS =====
  subject_id = sprintf("S%03d", 1:n),

  # ===== DEMOGRAPHICS =====
  age = round(rnorm(n, 45, 12)),
  gender = sample(c("Male", "Female"), n, replace = TRUE, prob = c(0.48, 0.52)),
  education = sample(c("High School", "Bachelor's", "Master's", "Doctorate"),
                    n, replace = TRUE, prob = c(0.3, 0.4, 0.2, 0.1)),
  income = sample(c("<30k", "30-60k", "60-100k", ">100k"),
                 n, replace = TRUE, prob = c(0.2, 0.35, 0.3, 0.15)),
  region = sample(c("North", "South", "East", "West"),
                 n, replace = TRUE, prob = c(0.25, 0.25, 0.25, 0.25)),

  # ===== INTERVENTION =====
  treatment_group = rep(c("Control", "Treatment"), each = n/2),
  adherence_pct = round(runif(n, 60, 100)),
  adverse_events = rbinom(n, 1, 0.15),

  # ===== OUTCOMES =====
  # Create large treatment effect (Cohen's d ≈ 2.1)
  baseline_score = round(rnorm(n, 50, 10)),

  # ===== PHYSIOLOGICAL MEASURES =====
  systolic_bp = round(rnorm(n, 120, 15)),
  diastolic_bp = round(rnorm(n, 80, 10)),
  cholesterol_total = round(rnorm(n, 200, 30)),
  cholesterol_ldl = round(rnorm(n, 120, 25)),
  cholesterol_hdl = round(rnorm(n, 50, 12)),
  bmi = round(rnorm(n, 26, 4), 1),
  glucose_fasting = round(rnorm(n, 95, 15)),
  heart_rate = round(rnorm(n, 72, 10)),

  # ===== BEHAVIORAL =====
  smoking_status = sample(c("Never", "Former", "Current"),
                         n, replace = TRUE, prob = c(0.5, 0.3, 0.2)),
  alcohol_drinks_week = rpois(n, 3),
  exercise_hours_week = round(rgamma(n, 2, 0.5), 1),
  sleep_hours_night = round(rnorm(n, 7, 1), 1),
  diet_quality = sample(c("Poor", "Fair", "Good", "Excellent"),
                       n, replace = TRUE, prob = c(0.15, 0.35, 0.35, 0.15)),

  # ===== PSYCHOLOGICAL =====
  anxiety_score = round(rnorm(n, 8, 4)),
  depression_score = round(rnorm(n, 7, 3)),
  stress_score = round(rnorm(n, 6, 3)),
  cognition_score = round(rnorm(n, 25, 5)),

  # ===== CLINICAL =====
  comorbidities_count = rpois(n, 1.5),
  medication_count = rpois(n, 2),

  # ===== SATISFACTION & QUALITY OF LIFE =====
  satisfaction_score = round(rnorm(n, 7, 2)),
  quality_of_life = round(rnorm(n, 75, 15)),

  # ===== TIME =====
  enrollment_date = seq(as.Date("2024-01-01"),
                       by = "3 days",
                       length.out = n),

  # ===== STUDY COMPLETION =====
  completed_study = rbinom(n, 1, 0.92),
  dropout_reason = sample(c(NA, "Adverse Event", "Lost to Follow-up", "Withdrew Consent"),
                         n, replace = TRUE,
                         prob = c(0.92, 0.03, 0.03, 0.02))
)

# Create follow-up score with large treatment effect
# Control group: slight improvement
# Treatment group: large improvement (effect size ≈ 2.1)
clinical_trial_data$followup_score <- with(clinical_trial_data, {
  ifelse(treatment_group == "Control",
         baseline_score + rnorm(n/2, 3, 5),  # Small improvement
         baseline_score + rnorm(n/2, 24, 5))  # Large improvement (Cohen's d ≈ 2.1)
})
clinical_trial_data$followup_score <- round(clinical_trial_data$followup_score)

# Create response category based on improvement
clinical_trial_data$response_category <- with(clinical_trial_data, {
  improvement <- followup_score - baseline_score
  cut(improvement,
      breaks = c(-Inf, 0, 10, 20, Inf),
      labels = c("No Response", "Minimal", "Moderate", "Excellent"))
})

# Add some correlations between related variables
# BMI correlates with blood pressure
clinical_trial_data$systolic_bp <- with(clinical_trial_data, {
  round(systolic_bp + (bmi - 26) * 1.5 + rnorm(n, 0, 5))
})

# Total cholesterol relates to LDL and HDL
clinical_trial_data$cholesterol_total <- with(clinical_trial_data, {
  round(cholesterol_ldl + cholesterol_hdl + rnorm(n, 30, 15))
})

# Exercise correlates negatively with BMI
clinical_trial_data$exercise_hours_week <- with(clinical_trial_data, {
  pmax(0, round(exercise_hours_week - (bmi - 26) * 0.2 + rnorm(n, 0, 1), 1))
})

# Anxiety and depression are correlated
clinical_trial_data$depression_score <- with(clinical_trial_data, {
  round(pmax(0, pmin(20, depression_score + anxiety_score * 0.3 + rnorm(n, 0, 2))))
})

# Add a few intentional outliers for testing outlier detection
outlier_indices <- sample(1:n, 5)
clinical_trial_data$systolic_bp[outlier_indices[1]] <- 180  # High BP
clinical_trial_data$bmi[outlier_indices[2]] <- 42  # High BMI
clinical_trial_data$cholesterol_total[outlier_indices[3]] <- 320  # High cholesterol
clinical_trial_data$age[outlier_indices[4]] <- 85  # Elderly
clinical_trial_data$heart_rate[outlier_indices[5]] <- 110  # High heart rate

# Ensure reasonable ranges
clinical_trial_data$age <- pmax(18, pmin(85, clinical_trial_data$age))
clinical_trial_data$systolic_bp <- pmax(90, pmin(200, clinical_trial_data$systolic_bp))
clinical_trial_data$diastolic_bp <- pmax(60, pmin(120, clinical_trial_data$diastolic_bp))
clinical_trial_data$bmi <- pmax(15, pmin(45, clinical_trial_data$bmi))
clinical_trial_data$sleep_hours_night <- pmax(4, pmin(12, clinical_trial_data$sleep_hours_night))
clinical_trial_data$anxiety_score <- pmax(0, pmin(20, clinical_trial_data$anxiety_score))
clinical_trial_data$depression_score <- pmax(0, pmin(20, clinical_trial_data$depression_score))
clinical_trial_data$stress_score <- pmax(0, pmin(15, clinical_trial_data$stress_score))
clinical_trial_data$cognition_score <- pmax(0, pmin(30, clinical_trial_data$cognition_score))
clinical_trial_data$satisfaction_score <- pmax(0, pmin(10, clinical_trial_data$satisfaction_score))
clinical_trial_data$quality_of_life <- pmax(0, pmin(100, clinical_trial_data$quality_of_life))
clinical_trial_data$baseline_score <- pmax(20, pmin(80, clinical_trial_data$baseline_score))
clinical_trial_data$followup_score <- pmax(20, pmin(100, clinical_trial_data$followup_score))

# Reorder columns for better organization
clinical_trial_data <- clinical_trial_data[, c(
  # Identifiers
  "subject_id",
  # Demographics
  "age", "gender", "education", "income", "region",
  # Intervention
  "treatment_group", "adherence_pct", "adverse_events",
  # Primary outcomes
  "baseline_score", "followup_score", "response_category",
  # Satisfaction
  "satisfaction_score", "quality_of_life",
  # Physiological
  "systolic_bp", "diastolic_bp",
  "cholesterol_total", "cholesterol_ldl", "cholesterol_hdl",
  "bmi", "glucose_fasting", "heart_rate",
  # Behavioral
  "smoking_status", "alcohol_drinks_week", "exercise_hours_week",
  "sleep_hours_night", "diet_quality",
  # Psychological
  "anxiety_score", "depression_score", "stress_score", "cognition_score",
  # Clinical
  "comorbidities_count", "medication_count",
  # Study administration
  "enrollment_date", "completed_study", "dropout_reason"
)]

# Save to data/ for package
usethis::use_data(clinical_trial_data, overwrite = TRUE)

# Also save as CSV to inst/extdata/ for examples
write.csv(clinical_trial_data,
          file = "inst/extdata/clinical_trial_data.csv",
          row.names = FALSE)

# Print summary
cat("\nClinical Trial Dataset Generated Successfully!\n")
cat("==================================================\n")
cat("Number of subjects:", nrow(clinical_trial_data), "\n")
cat("Number of variables:", ncol(clinical_trial_data), "\n")
cat("\nVariable types:\n")
cat("  Numeric:", sum(sapply(clinical_trial_data, is.numeric)), "\n")
cat("  Character:", sum(sapply(clinical_trial_data, is.character)), "\n")
cat("  Factor:", sum(sapply(clinical_trial_data, is.factor)), "\n")
cat("  Date:", sum(sapply(clinical_trial_data, function(x) inherits(x, "Date"))), "\n")
cat("\nTreatment effect (followup_score):\n")
cat("  Control mean:", round(mean(clinical_trial_data$followup_score[
  clinical_trial_data$treatment_group == "Control"]), 2), "\n")
cat("  Treatment mean:", round(mean(clinical_trial_data$followup_score[
  clinical_trial_data$treatment_group == "Treatment"]), 2), "\n")
cat("  Difference:", round(mean(clinical_trial_data$followup_score[
  clinical_trial_data$treatment_group == "Treatment"]) -
  mean(clinical_trial_data$followup_score[
    clinical_trial_data$treatment_group == "Control"]), 2), "\n")

# Calculate Cohen's d
control_scores <- clinical_trial_data$followup_score[
  clinical_trial_data$treatment_group == "Control"]
treatment_scores <- clinical_trial_data$followup_score[
  clinical_trial_data$treatment_group == "Treatment"]
pooled_sd <- sqrt((var(control_scores) + var(treatment_scores)) / 2)
cohens_d <- (mean(treatment_scores) - mean(control_scores)) / pooled_sd
cat("  Cohen's d:", round(cohens_d, 2), "\n")
cat("\n==================================================\n")
