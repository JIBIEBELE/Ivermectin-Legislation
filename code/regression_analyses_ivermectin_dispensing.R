################################################################################
#                                                                              #
# PROJECT: Changes in the scope & intent of US state legislation on ivermectin #
#          and pharmacy dispensing trends, 2020–2026                           #                                                                           #
# CODED BY: Jessica Ibiebele                                                   #
# DATE: October 13, 2026                                                       #
#                                                                              #
################################################################################

# Load packages
library(tidyverse)
library(readxl)
library(lubridate)
library(fixest)
library(zoo)

# ---------------------------------------------------------------------
# 1. Load and manipulate data
# ---------------------------------------------------------------------
combined_monthly_data <- read.csv("combined_monthly_data.csv")
pop_2020       <- read.csv("pop_2020.csv")
num_legbymonth <- read_excel("num_legbymonth.xlsx")
# num_legbymonth holds a RUNNING TOTAL of bills per state (each new bill
# carries forward to later months). A state's total is its FINAL (maximum) value. 
# The first month with a value >= 1 is the state's introduction month.

legislation_long <- num_legbymonth %>%
  pivot_longer(cols = matches("^leg_\\d{4}_\\d{2}$"),
               names_to = "month_str", values_to = "num_bills") %>%
  mutate(month_date = ym(str_remove(month_str, "^leg_")),
         state      = as.character(state),
         num_bills  = replace_na(num_bills, 0)) %>%
  select(state, month_date, num_bills)

leg_dates <- legislation_long %>%                 # first month with >= 1 bill
  filter(num_bills >= 1) %>%
  group_by(state) %>%
  summarize(intro_month = min(month_date), .groups = "drop")

data <- combined_monthly_data %>%
  mutate(state = as.character(state), month_date = as.Date(month_id)) %>%
  left_join(pop_2020, by = c("state" = "State")) %>%
  mutate(state_pop_100k = Population_2020 / 1e5) %>%
  left_join(leg_dates, by = "state") %>%
  mutate(
    period     = factor(period, levels = c("pre", "pandemic", "post")),
    intro_post = as.integer(!is.na(intro_month) & month_date >= intro_month),  # time-varying
    ever_leg   = as.integer(!is.na(intro_month)),                              # fixed label
    rate       = monthly_drug_sum / state_pop_100k
  )

# ---------------------------------------------------------------------
# 2. Examine differences in ivermectin dispensing rates among states who ever vs. never
#    introduced legislation, by time period (pre-, during, and post-COVID pandemic)
#    Note: pre-pandemic is the reference period
# ---------------------------------------------------------------------

# Tidy results function
tidy_rr <- function(m) {
  ct <- coeftable(m)
  tibble(term = rownames(ct), RR = exp(ct[, 1]),
         RR_L = exp(ct[, 1] - 1.96 * ct[, 2]),
         RR_U = exp(ct[, 1] + 1.96 * ct[, 2]), p = ct[, 4])
}
pull_row <- function(m, label, term = "intro_post", ...) {
  ct <- coeftable(m, ...)
  tibble(model = label, est = ct[term, 1], se = ct[term, 2],
         RR = exp(est), RR_L = exp(est - 1.96 * se), RR_U = exp(est + 1.96 * se))
}
pick <- function(m, label, pattern, ...) {
  ct <- coeftable(m, ...)
  ct <- ct[grepl(pattern, rownames(ct)), , drop = FALSE]
  tibble(model = label, term = rownames(ct), RR = exp(ct[, 1]),
         RR_L = exp(ct[, 1] - 1.96 * ct[, 2]), RR_U = exp(ct[, 1] + 1.96 * ct[, 2]))
}

# Model 1: Poisson
m_period <- fepois(monthly_drug_sum ~ period | state,
                   data = data, offset = ~log(state_pop_100k), cluster = ~state) # state-clustered SEs
print(tidy_rr(m_period), n = Inf)                         
summary(m_period)

## Model 2: Negative binomial 
m_period_nb <- fenegbin(monthly_drug_sum ~ period | state, data = d,
                        offset = ~log(state_pop_100k), cluster = ~state)
print(tidy_rr(m_period_nb), n = Inf)
summary(m_period_nb)

## Model 3: Interaction term between period * ever_legislated, Poisson
m_period_leg <- fepois(monthly_drug_sum ~ period * ever_leg | state,
                    data = data, offset = ~log(state_pop_100k), cluster = ~state)
print(tidy_rr(m_period_leg), n = Inf)
summary(m_period_leg)

## Model 4: Interaction term between period * ever_legislated, Negative binomial
## Note: NB gives smaller states more weight than Poisson
## Note: The ever_leg main effect is left out on purpose: it is
# redundant with the state FE; fepois drops it silently, but fenegbin does not,
# and keeping it prevents convergence
m_period_leg_nb <- fenegbin(monthly_drug_sum ~ period + i(period, ever_leg, ref = "pre") | state,
                      data = data, offset = ~log(state_pop_100k), cluster = ~state)
print(tidy_rr(m_period_leg_nb), n = Inf)
summary(m_period_leg_nb)

# Model 5: Pre-pandemic/baseline differences in dispensing rates between states that 
# later introduced legislation vs. those that never did, Poisson

m_pre_diff <- fepois(monthly_drug_sum ~ ever_leg, data = filter(data, period == "pre"),
                     offset = ~log(state_pop_100k), cluster = ~state)
print(tidy_rr(m_pre_diff), n = Inf)

# Model 6: Pre-pandemic/baseline differences in dispensing rates between states that 
# later introduced legislation vs. those that never did, Negative binomial
m_pre_diff_nb <- fenegbin(monthly_drug_sum ~ ever_leg, data = filter(data, period == "pre"),
                          offset = ~log(state_pop_100k), cluster = ~state)
print(tidy_rr(m_pre_diff_nb), n = Inf)
summary(m_pre_diff_nb)

# ---------------------------------------------------------------------
# 3. Treat legislation status as a time-varying indicator, where a state's
# legislative status switches from '0' to '1' in the month they introduced their
# first ivermectin-related bill
# ---------------------------------------------------------------------
f_main <- monthly_drug_sum ~ intro_post | state + month_date

m_main <- fepois(f_main, data = data, offset = ~log(state_pop_100k), cluster = ~state)
m_pois <- fepois(f_main, data = data, offset = ~log(state_pop_100k))      # naive SE for comparison
m_nb   <- fenegbin(f_main, data = data, offset = ~log(state_pop_100k))

print(tidy_rr(m_main), n = Inf)                    
print(tidy_rr(m_pois), n = Inf)   
print(tidy_rr(m_nb), n = Inf)   

# ---------------------------------------------------------------------
# 4. Sensitivity analysis: adjustment for state-level COVID-19 mortality
#    (separate from the primary model; the covariate is merged into d_covid)
# ---------------------------------------------------------------------
stopifnot("`d` is not a data frame: run section 1 first" = is.data.frame(data))

col_state  <- "state"
col_date   <- "week_ending_date"
col_deaths <- "covid_19_deaths"

raw_covid <- read.csv("covid19_monthly_deaths_state.csv") %>%
  rename(all_of(c(jurisdiction = col_state, week_end_raw = col_date,
                  death_count_suppressed = col_deaths)))
## Retrieved from the CDC National Center for Health Statistics 
## https://www.cdc.gov/nchs/nvss/vsrr/covid19/index.htm

deaths_chr <- as.character(raw_covid$death_count_suppressed)
deaths_num <- suppressWarnings(as.numeric(gsub(",", "", trimws(deaths_chr))))
cat("Non-numeric death values treated as suppressed:",
    unique(deaths_chr[is.na(deaths_num) & !is.na(deaths_chr) & trimws(deaths_chr) != ""]), "\n")
cat("Share of rows with missing/suppressed deaths:", round(mean(is.na(deaths_num)), 3), "\n")
raw_covid$death_count_suppressed <- deaths_num

# Rows with a blank week-ending date are summary rows: drop them (else double count)
blank <- is.na(raw_covid$week_end_raw) | trimws(as.character(raw_covid$week_end_raw)) == ""
cat("Summary rows dropped (blank week-ending date):", sum(blank), "of", nrow(raw_covid), "\n")
raw_covid <- raw_covid[!blank, ] %>%
  mutate(week_end   = as.Date(parse_date_time(substr(as.character(week_end_raw), 1, 10),
                                              orders = c("mdy", "mdY", "Ymd"))),
         week_start = floor_date(week_end, unit = "week", week_start = 1),
         month_date = floor_date(week_start, unit = "month"))     # month of the week's Monday
if (anyNA(raw_covid$month_date)) {
  print(head(unique(raw_covid$week_end_raw[is.na(raw_covid$month_date)]), 10))
  stop("Some week-ending dates did not parse; see values above")
}
cat("COVID series covers", format(min(raw_covid$month_date)), "to",
    format(max(raw_covid$month_date)), "\n")

state_key <- bind_rows(    # accept full names or abbreviations (50 states + DC)
  tibble(key = str_to_lower(c(state.name, "District of Columbia")), state = c(state.abb, "DC")),
  tibble(key = str_to_lower(c(state.abb, "DC")), state = c(state.abb, "DC")))

covid_state_month <- raw_covid %>%
  mutate(key = str_to_lower(str_trim(jurisdiction)),
         # NCHS reports New York City separately: combine so New York = state total.
         # VERIFY (if "New York" already includes NYC, delete this line).
         key = ifelse(key == "new york city", "new york", key)) %>%
  inner_join(state_key, by = "key") %>%                 # drops territories, US total
  group_by(state, month_date) %>%
  summarise(monthly_deaths = sum(death_count_suppressed, na.rm = TRUE),
            n_suppressed   = sum(is.na(death_count_suppressed)), .groups = "drop")

stopifnot(length(setdiff(unique(d$state), unique(covid_state_month$state))) == 0)
covid_start <- min(covid_state_month$month_date)

d_covid <- data %>%
  left_join(covid_state_month, by = c("state", "month_date")) %>%
  mutate(monthly_deaths = ifelse(is.na(monthly_deaths) & month_date < covid_start,
                                 0, monthly_deaths),    # before the series starts = 0
         monthly_deaths_per100k = monthly_deaths / state_pop_100k)
cat("Rows with missing COVID mortality:", sum(is.na(d_covid$monthly_deaths_per100k)), "\n")
stopifnot(sd(d_covid$monthly_deaths_per100k, na.rm = TRUE) > 0)   # failed-join guard

f_covid <- monthly_drug_sum ~ intro_post + monthly_deaths_per100k | state + month_date
m_covid    <- fepois(f_covid, data = d_covid, offset = ~log(state_pop_100k), cluster = ~state)
m_covid_nb <- fenegbin(f_covid, data = d_covid, offset = ~log(state_pop_100k), cluster = ~state)
print(tidy_rr(m_covid), n = Inf)       # includes the COVID-19 mortality coefficient
print(tidy_rr(m_covid_nb), n = Inf)


