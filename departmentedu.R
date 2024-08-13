# Load necessary libraries
library(splines)
library(ggplot2)

# Set seed for reproducibility
set.seed(123)

# Generate fake data
n <- 100  # Number of students
raw_assessments <- sort(runif(n, 50, 100))  # Raw assessments between 50 and 100
exam_marks <- raw_assessments + rnorm(n, mean = 0, sd = 10)  # Exam marks with some added noise
# probably would think about generating a more realistic distribution of students 

# Ensure exam marks are within 0 to 100 range 
exam_marks <- pmax(pmin(exam_marks, 100), 0)

# Plot the raw data
ggplot(data.frame(raw_assessments, exam_marks), aes(x = raw_assessments, y = exam_marks)) +
  geom_point() +
  labs(title = "Raw Assessments vs Examination Marks", x = "Raw Assessments", y = "Examination Marks")


# Linear Adjustment
linear_adjustment <- function(raw, exam) {
  a <- sd(exam) / sd(raw)
  b <- mean(exam) - a * mean(raw)
  return(a * raw + b)
}

# Apply the linear adjustment
moderated_linear <- linear_adjustment(raw_assessments, exam_marks)

# Plot the results
ggplot(data.frame(raw_assessments, exam_marks, moderated_linear), aes(x = raw_assessments)) +
  geom_point(aes(y = exam_marks), color = "blue") +
  geom_line(aes(y = moderated_linear), color = "red") +
  labs(title = "Linear Adjustment", x = "Raw Assessments", y = "Marks") +
  theme_minimal()


# Quadratic Polynomial Adjustment
quadratic_adjustment <- function(raw, exam) {
  # Fit a quadratic model
  fit <- lm(exam ~ poly(raw, 2, raw = TRUE))
  return(predict(fit, newdata = data.frame(raw = raw)))
}

# Apply the quadratic adjustment
moderated_quadratic <- quadratic_adjustment(raw_assessments, exam_marks)

# Plot the results
ggplot(data.frame(raw_assessments, exam_marks, moderated_quadratic), aes(x = raw_assessments)) +
  geom_point(aes(y = exam_marks), color = "blue") +
  geom_line(aes(y = moderated_quadratic), color = "red") +
  labs(title = "Quadratic Polynomial Adjustment", x = "Raw Assessments", y = "Marks") +
  theme_minimal()


# Quadratic Polynomial Adjustment
quadratic_adjustment <- function(raw, exam) {
  # Fit a quadratic model
  fit <- lm(exam ~ poly(raw, 2, raw = TRUE))
  return(predict(fit, newdata = data.frame(raw = raw)))
}

# Apply the quadratic adjustment
moderated_quadratic <- quadratic_adjustment(raw_assessments, exam_marks)

# Plot the results
ggplot(data.frame(raw_assessments, exam_marks, moderated_quadratic), aes(x = raw_assessments)) +
  geom_point(aes(y = exam_marks), color = "blue") +
  geom_line(aes(y = moderated_quadratic), color = "red") +
  labs(title = "Quadratic Polynomial Adjustment", x = "Raw Assessments", y = "Marks") +
  theme_minimal()

# MSR with a simple spline
msr_adjustment <- function(raw, exam) {
  # Fit a monotonic spline with one knot
  fit <- lm(exam ~ ns(raw, df = 2))
  return(predict(fit, newdata = data.frame(raw = raw)))
}

# Apply the MSR adjustment
moderated_msr <- msr_adjustment(raw_assessments, exam_marks)

# Plot the results
ggplot(data.frame(raw_assessments, exam_marks, moderated_msr), aes(x = raw_assessments)) +
  geom_point(aes(y = exam_marks), color = "blue") +
  geom_line(aes(y = moderated_msr), color = "red") +
  labs(title = "Monotone Spline Regression Adjustment", x = "Raw Assessments", y = "Marks") +
  theme_minimal()


# Combine the data for comparison
comparison <- data.frame(
  raw_assessments,
  exam_marks,
  moderated_linear,
  moderated_quadratic,
  moderated_msr
)

# Plot the comparison
ggplot(comparison, aes(x = raw_assessments)) +
  geom_point(aes(y = exam_marks), color = "blue", alpha = 0.5) +
  geom_line(aes(y = moderated_linear), color = "gray", linetype = "dashed", size = 0.7) +
  geom_line(aes(y = moderated_quadratic), color = "green", linetype = "dotted", size = 1) +
  geom_line(aes(y = moderated_msr), color = "purple", linetype = "solid", size = 1) +
  labs(title = "Comparison of Moderation Methods", x = "Raw Assessments", y = "Marks") +
  theme_minimal() +
  theme(legend.position = "bottom")





# Calculate Mean Squared Errors (MSE)
mse_linear <- mean((exam_marks - moderated_linear)^2)
mse_quadratic <- mean((exam_marks - moderated_quadratic)^2)
mse_msr <- mean((exam_marks - moderated_msr)^2)

# Calculate the standard deviation of differences
sd_diff_linear <- sd(exam_marks - moderated_linear)
sd_diff_quadratic <- sd(exam_marks - moderated_quadratic)
sd_diff_msr <- sd(exam_marks - moderated_msr)

# Print the results
mse_results <- data.frame(
  Method = c("Linear", "Quadratic", "MSR"),
  MSE = c(mse_linear, mse_quadratic, mse_msr),
  SD_Difference = c(sd_diff_linear, sd_diff_quadratic, sd_diff_msr)
)

print(mse_results)
# Combine the data for plotting
comparison <- data.frame(
  raw_assessments,
  exam_marks,
  moderated_linear,
  moderated_quadratic,
  moderated_msr
)
