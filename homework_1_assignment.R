# HOMEWORK 1 ----

# Libraries ----
library(tidyverse)
library(tidyquant)
library(readxl)
library(forcats)
library(stringr)

# Source Scripts ----
source("00_Scripts/assess_attrition.R")

# Data ----
path_train     <- "00_Data/telco_train.xlsx"
train_raw_tbl  <- read_excel(path_train, sheet = 1)

dept_jobrole_tbl <- train_raw_tbl %>%
    select(EmployeeNumber, Department, JobRole, PerformanceRating, Attrition)

kpi_industry_turnover_pct <- 0.088

# Productivity Cost by Role ----

productivity_cost_by_role_tbl <- read_excel("00_Data/productivity_cost_by_role.xlsx")
productivity_cost_by_role_tbl


# Q1: Which Job Role has the highest total cost of attrition? ----

dept_jobrole_productivity_tbl <- dept_jobrole_tbl %>%
    count(Department, JobRole, Attrition) %>%
    count_to_pct(Department, JobRole) %>%

    left_join(productivity_cost_by_role_tbl) %>%

    assess_attrition(Attrition, attrition_value = "Yes", baseline_pct = kpi_industry_turnover_pct) %>%
    mutate(attrition_cost = calculate_attrition_cost(n = n,
                                                    salary = Salary_Average,
                                                     net_revenue_per_employee = Revenue_Average))

dept_jobrole_productivity_tbl %>%
    plot_attrition(Department, JobRole, .value = attrition_cost,
                   units = "M") +
    labs(title = "Estimated Attrition Cost by Job Role",
         x = "Attrition Cost",
         subtitle = "Sales Executives have the highest attrition cost, causing a huge organizational hit")

# Q2: What is the total cost of attrition for the Research & Development: Research Scientist job role? ----

dept_jobrole_productivity_tbl %>%
    group_by(Department, JobRole, Attrition) %>%
    filter(JobRole == "Research Scientist") %>%
    ungroup() %>%
    select(JobRole, attrition_cost)

# Q3: What percentage do the top four Job Roles account for in terms of the total cost of attrition? ----

dept_jobrole_productivity_tbl %>%
    arrange(desc(attrition_cost)) %>%
    mutate(row_num = row_number()) %>%
    mutate(is_top_4 = case_when(
        row_num <= 4 ~ "Yes",
        TRUE ~ "No"
    )) %>%

    group_by(is_top_4) %>%
    summarize(total_attrition_cost = sum(attrition_cost)) %>%
    ungroup() %>%

    mutate(total_attrition_cost_pct = total_attrition_cost / sum(total_attrition_cost))

# Q4. Which Department has the highest total cost of attrition? ----

dept_jobrole_productivity_tbl %>%
    group_by(Department) %>%
    summarize(attrition_cost_by_dept = sum(attrition_cost)) %>%
    arrange(desc(attrition_cost_by_dept)) %>%
    ungroup()

# Q5: What percentage does the top Department account for in terms of the total cost of attrition? ----

dept_jobrole_productivity_tbl %>%
    group_by(Department) %>%
    summarize(attrition_cost_by_dept = sum(attrition_cost)) %>%
    arrange(desc(attrition_cost_by_dept)) %>%
    summarize(attrition_cost_pct_by_dept = attrition_cost_by_dept / sum(attrition_cost_by_dept)) %>%
    ungroup()
