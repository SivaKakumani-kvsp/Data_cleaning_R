install.packages("tidyverse") 
install.packages("dplyr")
library(dplyr)
library(tidyverse)
library(readxl)


setwd("C:/Users/kvsp1/Desktop/DataScience")
df <- read_excel("NationalSalaries.xlsx")
df2 <- read_excel("Salaries.xlsx", guess_max = 10000)

View(df)  
view(df2)
summary(df)

#column names
colnames(df)

#for loop for finding the index of invalid columns
for (i in 1:nrow(df)) {
  for (j in 1:ncol(df)) {
    if (!is.na(df[i, j]) && (df[i, j] == "**" || df[i, j] == "#" || df[i, j] == "*"||df[i,j]=="***"||df[i,j]==" ")) {
      
      print(j)
    }
  }
}

#remove the invalid rows
df_removed <- df %>% filter(!(TotalEmployment == "#" | TotalEmployment == "*"|TotalEmployment == "**" | TotalEmployment == "***"|TotalEmployment == " ")& 
                                  !(EMP_PRSE == "#" | EMP_PRSE == "*"|EMP_PRSE == "**" | EMP_PRSE == "***"|EMP_PRSE == " ")&
                                  !(H_MEAN == "#" | H_MEAN == "*"|H_MEAN == "**" | H_MEAN == "***"|H_MEAN == " ")& 
                                  !(A_MEAN == "#" | A_MEAN == "*"|A_MEAN == "**" | A_MEAN == "***"|A_MEAN == " ")& 
                                  !(MEAN_PRSE == "#" | MEAN_PRSE == "*"|MEAN_PRSE == "**" | MEAN_PRSE == "***"|MEAN_PRSE == " ")& 
                                  !(H_PCT10 == "#" | H_PCT10 == "*"|H_PCT10 == "**" | H_PCT10 == "***"|H_PCT10 == " ")& 
                                  !(H_PCT25 == "#" | H_PCT25 == "*"|H_PCT25 == "**" | H_PCT25 == "***"|H_PCT25 == " ")& 
                                  !(H_MEDIAN == "#" | H_MEDIAN == "*"|H_MEDIAN == "**" | H_MEDIAN == "***"|H_MEDIAN == " ")& 
                                  !(H_PCT75 == "#" | H_PCT75 == "*"|H_PCT75 == "**" | H_PCT75 == "***"|H_PCT75 == " ")& 
                                  !(H_PCT90 == "#" | H_PCT90 == "*"|H_PCT90 == "**" | H_PCT90 == "***"|H_PCT90 == " ")& 
                                  !(A_PCT10 == "#" | A_PCT10 == "*"|A_PCT10 == "**" | A_PCT10 == "***"|A_PCT10 == " ")& 
                                  !(A_PCT25 == "#" | A_PCT25 == "*"|A_PCT25 == "**" | A_PCT25 == "***"|A_PCT25 == " ")& 
                                  !(A_PCT75 == "#" | A_PCT75 == "*"|A_PCT75 == "**" | A_PCT75 == "***"|A_PCT75 == " ")& 
                                  !(A_PCT90 == "#" | A_PCT90 == "*"|A_PCT90 == "**" | A_PCT90 == "***"|A_PCT90 == " ")& 
                                  !(A_MEDIAN == "#" | A_MEDIAN == "*"|A_MEDIAN == "**" | A_MEDIAN == "***"|A_MEDIAN == " "))

                                   
                          

#-------------------------------------------------------
#2.Select only columns that appear in the Salaries.xlsx file. Save the result into a new 
#file and use the new file to complete the remaining tasks below.(10')

#in this question I change the headers to make it matches with other dataset.

similar_cols <- intersect(colnames(df_removed), colnames(df2))
df_similar_cols <- df2[, similar_cols]
# Write new data set
write.csv(df_similar_cols, "combined_columns.csv", row.names=FALSE, fileEncoding = "UTF-8", na = ' ')
# Read csv file to perform following tasks
df_combined <- read.csv(file = "combined_columns.csv",header = TRUE,sep = ",",dec = ".",stringsAsFactors = FALSE)


#-----------------------------
#3.Randomly select 1500 rows. (10')

df_random <- df_combined %>%
  sample_n(1500)

#----------------------------
#Create a data frame that holds only individual jobs (not major groups or all 
#occupations) whose average hourly salary is lower than 15. (
df_filtered_individ <- df_combined %>%
  filter((Group != "major" | JobName != "All Occupations") & AverageHourlySalary < 15)
#------------------------
#5.5. Create a data frame that holds only individual jobs (not major groups or all 
#occupations) in Indiana, then divide average yearly salary range into 10 intervals(bins), 
#and count how many jobs are in each bin. (10')
df_filtered_indiana <- df2 %>%
  filter((Group != "major" | JobName != "All Occupations") & StateName=="Indiana")

# the cut function is used to split the values in the "yearly wage" column into 10 quantile-based bins, and a new column "bin" is created to house the bin values.
df_filtered_indiana$bin <- cut(df_filtered_indiana$AverageYearlySalary, 
                      breaks = quantile(df_filtered_indiana$AverageYearlySalary, 
                                        probs = seq(0, 1, length.out = 11)), include.lowest = TRUE)

df_counts <- df_filtered_indiana %>%
  group_by(bin) %>%
  summarize(count = n())
#-------------------------
#6. Find the total employment for each state.
df_total_employment_state <- df2 %>%
  group_by(StateName) %>%
  summarize(total_employment = sum(TotalEmployment))

#7. Find the average yearly salary of all jobs in Indiana, and compare it with data 
#provided in the data set (42630 vs 36410). (20’)
df_indiana_jobs <- df2 %>%
  filter(StateName == "Indiana")

average_yearly_salary_indiana <- df_indiana_jobs %>%
  summarize(average_yearly_salary = mean(AverageYearlySalary))

#comparing the average salary with 42630 and 36410
df_IN_jobs_g <- df_indiana_jobs %>%
  filter(AverageYearlySalary > 42630)

df_IN_jobs_l <- df_indiana_jobs %>%
  filter(AverageYearlySalary < 42630)

df_IN_jobs_l3 <- df_indiana_jobs %>%
  filter(AverageYearlySalary < 36410)

df_IN_jobs_g3 <- df_indiana_jobs %>%
  filter(AverageYearlySalary > 36410)

df_IN_jobs_gl <- df_indiana_jobs %>%
  filter(AverageYearlySalary > 36410 & AverageYearlySalary < 42630)
df_IN_jobs_avg_lg36410 <- df_indiana_jobs %>%
  filter(AverageYearlySalary > 36410 & AverageYearlySalary < mean(AverageYearlySalary))

df_IN_jobs_avg_gl42630 <- df_indiana_jobs %>%
  filter(AverageYearlySalary > mean(AverageYearlySalary) & AverageYearlySalary < 42630)

#---------------------------------
#8. Use a chart to compare average yearly salaries of "Computer and mathematical 
#occupations" (coded 15 - xxxx) in Indiana, California and New York. Use colors and 
#legends to make your chart informative. (10’)
library(ggplot2)



df_jobs_3S <- df2 %>%
  filter(JobName == "Computer and mathematical occupations" & JobCode == "15-0000" &
           StateName %in% c("Indiana", "California", "New York"))

ggplot(df_jobs_3S, aes(x = StateName, y = AverageYearlySalary, fill=StateName)) +
  geom_col(show.legend = FALSE) +
  labs(x="StateName", y="Average Yearly Salary", title="Average Yearly Salary of Computer and Mathematical Occupations of Indiana California and New York") +
  scale_fill_manual(values=c("Indiana"="orange", "California"="grey", "New York"="skyblue")) +
  theme(plot.title = element_text(hjust = 0.5))

# to clear the environment
rm(list = ls(all.names = TRUE))
view(ACC)

#to clear the console
cat("\014")
