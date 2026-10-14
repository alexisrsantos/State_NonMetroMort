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
I reran the green line with every suppressed cell set to 0 deaths, and then to 9, the two extremes. In 89% of state-years the result changed by less than 1 point on the MRR scale. Only Massachusetts and Utah moved noticeably, because their nonmetro minority populations are very small. This has been recognized in the methods.
