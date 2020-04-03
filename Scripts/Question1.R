outcome <- read.csv(".\\Data\\outcome-of-care-measures.csv"
                    , colClasses = "character")
#head(outcome)
#ncol(outcome)
names(outcome)

outcome[, 11] <- as.numeric(outcome[, 11])
hist(outcome[, 11])
