#Q1
source(".\\Scripts\\best.R")
best("SC", "heart attack")

#Q2
source(".\\Scripts\\best.R")
best("NY", "pneumonia")

#Q3
source(".\\Scripts\\best.R")
best("AK", "pneumonia")

#Q4
source(".\\Scripts\\rankhospital.R")
rankhospital("NC", "heart attack", "worst")

#Q5
source(".\\Scripts\\rankhospital.R")
rankhospital("WA", "heart attack", 7)

#Q6
source(".\\Scripts\\rankhospital.R")
rankhospital("TX", "pneumonia", 10)

#Q7
source(".\\Scripts\\rankhospital.R")
rankhospital("NY", "heart attack", 7)

#Q8
source(".\\Scripts\\rankall.R")
r <- rankall("heart attack", 4)
as.character(subset(r, state == "HI")$hospital)

#Q9
source(".\\Scripts\\rankall.R")
r <- rankall("pneumonia", "worst")
as.character(subset(r, state == "NJ")$hospital)

#Q10
source(".\\Scripts\\rankall.R")
r <- rankall("heart failure", 10)
as.character(subset(r, state == "NV")$hospital)
