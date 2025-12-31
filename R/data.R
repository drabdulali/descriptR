#' Synthetic Clinical Trial Dataset
#'
#' A comprehensive synthetic dataset from a randomized controlled trial (RCT)
#' with 100 subjects and 41 variables. This dataset is designed to demonstrate
#' all features of the descriptR package including descriptive statistics,
#' inferential tests, visualizations, and export capabilities.
#'
#' @format A data frame with 100 rows and 41 variables:
#'
#' **Identifiers**
#' \describe{
#'   \item{subject_id}{Unique subject identifier (S001-S100)}
#' }
#'
#' **Demographics**
#' \describe{
#'   \item{age}{Age in years (18-85)}
#'   \item{gender}{Gender (Male, Female)}
#'   \item{education}{Education level (High School, Bachelor's, Master's, Doctorate)}
#'   \item{income}{Annual income bracket (<30k, 30-60k, 60-100k, >100k)}
#'   \item{region}{Geographic region (North, South, East, West)}
#' }
#'
#' **Intervention**
#' \describe{
#'   \item{treatment_group}{Study group assignment (Control, Treatment)}
#'   \item{adherence_pct}{Treatment adherence percentage (60-100)}
#'   \item{adverse_events}{Binary indicator of adverse events (0 = No, 1 = Yes)}
#' }
#'
#' **Primary Outcomes**
#' \describe{
#'   \item{baseline_score}{Primary outcome at baseline (20-80)}
#'   \item{followup_score}{Primary outcome at follow-up (20-100)}
#'   \item{response_category}{Treatment response category (No Response, Minimal, Moderate, Excellent)}
#' }
#'
#' **Satisfaction & Quality of Life**
#' \describe{
#'   \item{satisfaction_score}{Patient satisfaction score (0-10)}
#'   \item{quality_of_life}{Quality of life score (0-100)}
#' }
#'
#' **Physiological Measures**
#' \describe{
#'   \item{systolic_bp}{Systolic blood pressure (mmHg)}
#'   \item{diastolic_bp}{Diastolic blood pressure (mmHg)}
#'   \item{cholesterol_total}{Total cholesterol (mg/dL)}
#'   \item{cholesterol_ldl}{LDL cholesterol (mg/dL)}
#'   \item{cholesterol_hdl}{HDL cholesterol (mg/dL)}
#'   \item{bmi}{Body mass index (kg/m²)}
#'   \item{glucose_fasting}{Fasting glucose (mg/dL)}
#'   \item{heart_rate}{Resting heart rate (bpm)}
#' }
#'
#' **Behavioral Measures**
#' \describe{
#'   \item{smoking_status}{Smoking status (Never, Former, Current)}
#'   \item{alcohol_drinks_week}{Alcoholic drinks per week}
#'   \item{exercise_hours_week}{Exercise hours per week}
#'   \item{sleep_hours_night}{Average sleep hours per night}
#'   \item{diet_quality}{Self-reported diet quality (Poor, Fair, Good, Excellent)}
#' }
#'
#' **Psychological Measures**
#' \describe{
#'   \item{anxiety_score}{Anxiety assessment score (0-20)}
#'   \item{depression_score}{Depression assessment score (0-20)}
#'   \item{stress_score}{Stress assessment score (0-15)}
#'   \item{cognition_score}{Cognitive function score (0-30)}
#' }
#'
#' **Clinical Information**
#' \describe{
#'   \item{comorbidities_count}{Number of comorbid conditions}
#'   \item{medication_count}{Number of concurrent medications}
#' }
#'
#' **Study Administration**
#' \describe{
#'   \item{enrollment_date}{Study enrollment date}
#'   \item{completed_study}{Study completion indicator (0 = No, 1 = Yes)}
#'   \item{dropout_reason}{Reason for dropout if applicable (NA, Adverse Event, Lost to Follow-up, Withdrew Consent)}
#' }
#'
#' @details
#' ## Dataset Characteristics
#'
#' This synthetic dataset was designed with the following features:
#'
#' **Treatment Effect**
#' - Large treatment effect (Cohen's d ≈ 2.1) on followup_score
#' - Control group: mean improvement ≈ 3 points from baseline
#' - Treatment group: mean improvement ≈ 24 points from baseline
#'
#' **Variable Correlations**
#' - BMI correlates with blood pressure
#' - Total cholesterol relates to LDL and HDL
#' - Exercise correlates negatively with BMI
#' - Anxiety and depression scores are correlated
#' - Related physiological measures show expected patterns
#'
#' **Data Quality Features**
#' - Complete data (no missing values) for demonstration purposes
#' - 5 intentional outliers for testing outlier detection:
#'   - High blood pressure (180 mmHg)
#'   - High BMI (42 kg/m²)
#'   - High cholesterol (320 mg/dL)
#'   - Elderly subject (85 years)
#'   - Elevated heart rate (110 bpm)
#' - Mix of normal and non-normal distributions
#' - Realistic value ranges for all measures
#'
#' **Statistical Testing Opportunities**
#' - T-tests: Compare treatment groups on continuous outcomes
#' - ANOVA: Compare across education levels or regions
#' - Chi-square: Test associations between categorical variables
#' - Correlation: Examine relationships between physiological measures
#' - Regression: Predict outcomes from multiple predictors
#' - Survival analysis: Study completion and dropout
#'
#' ## Example Usage
#'
#' \preformatted{
#' # Load the dataset
#' data(clinical_trial_data)
#'
#' # Basic descriptive statistics
#' describe_data(clinical_trial_data)
#'
#' # Compare treatment groups
#' compare_groups(clinical_trial_data,
#'                outcome = "followup_score",
#'                group = "treatment_group")
#'
#' # Analyze demographics
#' describe_categorical(clinical_trial_data,
#'                     vars = c("gender", "education", "region"))
#'
#' # Examine physiological measures
#' describe_numeric(clinical_trial_data,
#'                 vars = c("systolic_bp", "cholesterol_total", "bmi"))
#'
#' # Visualize treatment effect
#' visualize_descriptives(clinical_trial_data,
#'                       vars = "followup_score",
#'                       group_by = "treatment_group",
#'                       color_scheme = "medium")
#'
#' # Check for outliers
#' detect_outliers(clinical_trial_data,
#'                vars = c("systolic_bp", "bmi", "cholesterol_total"))
#'
#' # Assess normality
#' assess_normality(clinical_trial_data,
#'                 vars = c("baseline_score", "followup_score"))
#' }
#'
#' @source Synthetic data generated specifically for the descriptR package.
#'   See \code{data-raw/generate_clinical_trial_data.R} for generation code.
#'
#' @references
#' Ali, A. (2025). descriptR: Comprehensive Descriptive Statistics with
#' Automated Insights. R package version 0.1.0.
#'
#' @seealso
#' \code{\link{describe_data}}, \code{\link{describe_grouped}},
#' \code{\link{compare_groups}}, \code{\link{visualize_descriptives}}
#'
#' @examples
#' # Load the dataset
#' data(clinical_trial_data)
#'
#' # View structure
#' str(clinical_trial_data)
#'
#' # Summary statistics
#' summary(clinical_trial_data)
#'
#' # First few rows
#' head(clinical_trial_data)
#'
#' # Treatment effect
#' with(clinical_trial_data, {
#'   tapply(followup_score, treatment_group, mean)
#' })
#'
#' @keywords datasets
"clinical_trial_data"
