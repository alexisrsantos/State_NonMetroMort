# Decomposition 

# Processing raw files

## Suppressed cells
The data extracts include *Suppressed cells*. These were fewer than 10 deaths in the state-year-area: 1,013 cells (~9%).

WONDER left these rows out of your downloads entirely. 

For each one, I performed the following estimations and adjustments: 

1. Population, recovered exactly. The five groups always add up to the overall file’s population, so the missing group’s population is the total minus the groups that were reported. When more than one group was missing in the same state-year-area, I split the remainder using each group’s population in neighboring years, or its national share if it was never reported.
   
2. Deaths, estimated. I used that group’s average death rate in the same state and area across the years where WONDER did report it, times the recovered population, capped at 9, since the true count must be 0–9. If the group was never reported there, I used its national rate for that area type.
   
3. Age-adjusted rate, estimated. The crude rate using the data from point 1 and 2, times that group’s typical ratio of age-adjusted to crude rate in the years where both were reported.

## Unreliable cells
4. *Unreliable* cells, these are state-year-area counts ranging between 10 and 19 deaths: 889 cells (about ~8%)

WONDER reports the deaths and population for these but hides the age-adjusted rate. It still gives the 95% CI, so I used the midpoint of the CI. 
I tested on cells where the true rate is known, this lands within about 3% of it, and I added a small correction (×1.029) for its slight downward bias.

# Data checks
The status column in the dataset marks each cell as reported, aamr_unreliable or suppressed.

# Are results sensitive?
I reran the green line with every suppressed cell set to 0 deaths, and then to 9, the two extremes. In 89% of state-years the result changed by less than 1 point on the MRR scale. Only Massachusetts and Utah moved noticeably, because their nonmetro minority populations are very small. This has been recognized in the methods. For Massachusetts, I only present the observed ratio. 

## Codebook 

The file `nmp_analysis_data_aamr` contains the data used for the Kitagawa decomposition of the nonmetropolitan mortality penalty. Each row represents one ethnoracial group in one area type (metro or nonmetro) of one state in one year. The file has 10,890 rows covering the 50 states and the District of Columbia from 1999 to 2020. New Jersey, Rhode Island and the District of Columbia have no nonmetro counties under the 2006 classification, so they appear only as metro and are excluded from the decomposition.

All data come from CDC WONDER, Underlying Cause of Death 1999–2020, for all causes of death, grouped by state and year. Counties are classified with the 2006 NCHS Urban–Rural Classification Scheme. Rates are expressed per 100,000 population and age-adjusted to the 2000 U.S. standard population.

**`state`** is the name of the state.

**`year`** is the calendar year, from 1999 to 2020.

**`area`** is the county type. `metro` includes Large Central, Large Fringe, Medium and Small Metro counties; `nonmetro` includes Micropolitan and NonCore counties.

**`group`** is the ethnoracial group: non-Hispanic White (`NH White`), non-Hispanic Black (`NH Black`), non-Hispanic American Indian or Alaska Native (`NH AIAN`), non-Hispanic Asian or Pacific Islander (`NH API`), and Hispanic of any race (`Hispanic`).

**`deaths`** is the number of deaths in the group. It is reported by CDC WONDER, except for suppressed cells, where it is estimated (see `status`).

**`pop`** is the population of the group. It is reported by CDC WONDER, except for suppressed cells, where it is recovered from the area totals.

**`aamr`** is the group's age-adjusted death rate per 100,000. How it was obtained depends on `status`.

**`se_log_aamr`** is the standard error of the logarithm of `aamr`. It is derived from the 95% confidence interval reported by CDC WONDER as [ln(upper) − ln(lower)] / (2 × 1.96). Where no interval was reported, a Poisson approximation (1/√deaths) is used.

**`aa_factor`** is the typical ratio of the age-adjusted to the crude death rate for the same state, area and group, calculated as the median over years with reported rates. It is used only to estimate `aamr` for suppressed cells.

**`status`** indicates how the values in each row were obtained:

- `reported` (8,988 rows): all values were reported by CDC WONDER.
- `aamr_unreliable` (889 rows): the group had 10–19 deaths. Deaths and population were reported, but CDC WONDER does not display the age-adjusted rate. `aamr` was estimated as the geometric midpoint of the reported 95% confidence interval, multiplied by 1.029 to correct for a small downward bias estimated on cells with 20–39 deaths.
- `suppressed` (1,013 rows): the group had fewer than 10 deaths and CDC WONDER did not report the cell. Population was recovered as the area total minus the reported groups. Deaths were estimated from the group's death rate in other years for the same state and area, capped at 9. `aamr` was estimated as the crude rate multiplied by `aa_factor`.

**`total_deaths`** is the total number of deaths in the state, year and area, taken from the overall extract. It includes deaths with Hispanic origin not stated.

**`total_pop`** is the total population of the state, year and area. It equals the sum of `pop` across the five groups.

**`total_aamr`** is the overall age-adjusted death rate of the state, year and area. It is used to compute the observed mortality rate ratio.

**`total_aa_lo`** and **`total_aa_hi`** are the lower and upper limits of the 95% confidence interval for `total_aamr`, as reported by CDC WONDER.

The `total_*` variables have the same value in all five group rows of a given state, year and area.
