library(readxl)

df <- read_excel("C:/Users/Dhruv/Downloads/Single proportion test.xlsx")

female_sales <- sum(df$Department == "Sales" & df$Gender == "Female")
female_rd <- sum(df$Department == "R&D" & df$Gender == "Female")

total_sales <- sum(df$Department == "Sales")
total_rd <- sum(df$Department == "R&D")

prop_female_sales <- female_sales / total_sales
prop_female_sales
prop_female_rd <- female_rd / total_rd
prop_female_rd

test_result <- prop.test(c(female_sales, female_rd), c(total_sales, total_rd), alternative = "less")

cat("\nTest: Proportion of Females in Sales < R&D\n")
cat("Z-score:", sqrt(test_result$statistic), "\n")
cat("P-value:", test_result$p.value, "\n")

if (test_result$p.value < 0.05) {
  cat("Reject the null hypothesis: The proportion of females in Sales is significantly less than in R&D.\n")
} else {
  cat("Accept the null hypothesis: The proportion of females in Sales and R&D is approximately equal.\n")
}

