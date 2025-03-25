library(readxl)

df <- read_excel("C:/Users/Dhruv/Downloads/Single proportion test.xlsx")

#H0=Proportion of males and females in sales department is equal.
#H1=Proportion of males and females in sales department is unequal.

sales_male <- sum(df$Department == "Sales" & df$Gender == "Male")
cat("Number of Males in Sales: ",sales_male)
sales_female <- sum(df$Department == "Sales" & df$Gender == "Female")
cat("Number of Females in Sales: ",sales_female)
cat("Total Number of people in sales: ",sum(df$Department == "Sales"))

prop_test <- function(male, female, department) {
  total <- male + female
  p_hat <- male / total
  p_0 <- 0.5
  se <- sqrt(p_0 * (1 - p_0) / total)
  z_score <- (p_hat - p_0) / se
  p_value <- 2 * (1 - pnorm(abs(z_score)))
  
  cat("Z-Test for", department, "Department: \n")
  cat("Z-score:", z_score,"\n")
  cat("P-value:", p_value,"\n")
  
  if (p_value < 0.05) {
    cat("Result: Reject the null hypothesis \n")
  } else {
    cat("Result: Accept the null hypothesis \n")
  }
}

prop_test(sales_male, sales_female, "Sales")



