library(readxl)
library(writexl)   # install.packages("writexl") if needed
library(dplyr)
library(tidyr)

setwd("") #Set WD to read file from there

#### 1. Read data ####
# One row per state x year x area (metro / nonmetro) x ethnoracial group
DataFile <- read_excel("nmp_analysis_data_aamr.xlsx", sheet = "Data")

#### 2. Kitagawa decomposition (age-adjusted rates) ####
Wide <- DataFile %>%
  group_by(state, year, area) %>%
  mutate(Share = pop / sum(pop),                              # ethnoracial composition
         Var   = (aamr * se_log_aamr)^2) %>%                  # variance of group AAMR
  ungroup() %>%
  select(state, year, area, group, Share, aamr, Var, total_aamr, total_aa_lo, total_aa_hi) %>%
  pivot_wider(names_from = area, values_from = c(Share, aamr, Var, total_aamr, total_aa_lo, total_aa_hi)) %>%
  filter(!is.na(Share_nonmetro), !is.na(Share_metro))       # NJ, RI, DC have no nonmetro counties

Decomp <- Wide %>%
  mutate(Share_avg = (Share_nonmetro + Share_metro) / 2) %>%  # common composition
  group_by(state, year) %>%
  summarise(
    AAMR_N    = first(total_aamr_nonmetro),                   # published AAMRs
    AAMR_M    = first(total_aamr_metro),
    SElog_N   = (log(first(total_aa_hi_nonmetro)) - log(first(total_aa_lo_nonmetro))) / (2 * 1.96),
    SElog_M   = (log(first(total_aa_hi_metro))    - log(first(total_aa_lo_metro)))    / (2 * 1.96),
    Rebuilt_N = sum(Share_nonmetro * aamr_nonmetro),          # AAMR rebuilt from groups
    Rebuilt_M = sum(Share_metro    * aamr_metro),
    Std_N     = sum(Share_avg * aamr_nonmetro),               # AAMR with composition held equal
    Std_M     = sum(Share_avg * aamr_metro),
    VarStd_N  = sum(Share_avg^2 * Var_nonmetro),
    VarStd_M  = sum(Share_avg^2 * Var_metro),
    .groups = "drop") %>%
  mutate(
    MRR        = 100 * AAMR_N / AAMR_M,                                  # blue line
    MRR_Adj    = MRR * (Std_N / Rebuilt_N) / (Std_M / Rebuilt_M),        # green line
    SElog      = sqrt(SElog_N^2 + SElog_M^2),
    SElog_Adj  = sqrt(VarStd_N / Std_N^2 + VarStd_M / Std_M^2),
    MRR_Lo     = MRR * exp(-1.96 * SElog),     MRR_Hi     = MRR * exp(1.96 * SElog),
    MRR_Lo_Adj = MRR_Adj * exp(-1.96 * SElog_Adj), MRR_Hi_Adj = MRR_Adj * exp(1.96 * SElog_Adj))

#### 3. Export: State, Year, MRR, MRR_Adj and CIs, 1999-2019 ####
Export <- Decomp %>%
  filter(year <= 2019) %>%
  select(State = state, Year = year, MRR, MRR_Adj, MRR_Lo, MRR_Hi, MRR_Lo_Adj, MRR_Hi_Adj) %>%
  arrange(State, Year)

#Massachusetts: adjusted ratio not reliable (suppressed nonmetro counts) -> set to missing
Export[Export$State == "Massachusetts", c("MRR_Adj", "MRR_Lo_Adj", "MRR_Hi_Adj")] <- NA

write_xlsx(Export, "State_Year_1999_2019.xlsx")
