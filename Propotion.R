library(readxl)
df <- read_excel("C:/Users/Dhruv/Downloads/Single proportion test.xlsx")

male_sales <- sum(df$Department == "Sales" & df$Gender == "Male")
male_sales
male_hr <- sum(df$Department == "HR" & df$Gender == "Male")
male_hr

total_sales <- sum(df$Department == "Sales")
total_sales
total_hr <- sum(df$Department == "HR")
total_hr

prop_male_sales <- male_sales / total_sales
prop_male_sales
prop_male_hr <- male_hr / total_hr
prop_male_hr

test_result <- prop.test(c(male_sales, male_hr), c(total_sales, total_hr), alternative = "greater")

cat("\nTest: Proportion of Males in Sales > HR\n")
cat("Z-score:", sqrt(test_result$statistic), "\n")
cat("P-value:", test_result$p.value, "\n")

if (test_result$p.value < 0.05) {
  cat("Reject the null hypothesis: The proportion of males in Sales is significantly greater than in HR.\n")
} else {
  cat("Accept the null hypothesis: The proportion of males in Sales and HR is approximately equal.\n")
}

