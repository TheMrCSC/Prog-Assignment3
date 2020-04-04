rankhospital <- function(state, outcome, num = "best"){
        
        #reading all the data in the csv file
        complete_data <- read.csv(".\\Data\\outcome-of-care-measures.csv")
        
        #translating outcome to column name
        #1. Splitting outcome string
        outcome_split <- strsplit(outcome, " ")[[1]]
        
        #print("outcome_split")
        #2. taking a substring of the 1 character, Uppercasing it, 
        #       pasting it with a substring of the rest of the characters
        outcome_final <- paste(toupper(substring(outcome_split, 1,1)), 
                               substring(outcome_split, 2),
                               sep="", collapse=".")
        
        #3. getting column names
        col_name <- paste("Hospital.30.Day.Death..Mortality..Rates.from",
                          outcome_final,sep=".")
        
        #print(col_name)
        #checking validity of outcome
        #checking if the columname derived exists in names of the dataset
        invalid_outcome <- !(col_name %in% names(complete_data))
        
        #checking validity of the state
        #checking if state exists in the list of unique states from the dataset
        #using TRUE == 1 property to validate the same
        invalid_state <- !(sum(state == unique(complete_data["State"]) ) == 1)
        
        if (invalid_state){
                #if state is invalid stop the process
                stop("invalid state")
                
        }else if(invalid_outcome){
                #if outcome is invalid stop the process
                stop("invalid outcome")
                
        } else {
                #if the inputs are valid, getting the list of all Hospitals in 
                #state with all the outcomes
                hospital_state_outcome <- complete_data[
                        complete_data$State == state & 
                                complete_data[col_name] != "Not Available",
                        c("Hospital.Name", "State",col_name)]
                colnames(hospital_state_outcome)[3] <- "Rate"
                hospital_state_outcome$Rate <- as.numeric(as.character(
                        hospital_state_outcome$Rate))
                #getting rank of all hospitals in the state
                ordered_hospitals <- hospital_state_outcome[order(
                        hospital_state_outcome$Rate
                        ,hospital_state_outcome$Hospital.Name),]
                #print(ordered_hospitals)
                #returning valsues as per the requested rank
                if (num == "best"){
                        return(ordered_hospitals[1,1])
                }else if (num == "worst"){
                        num_worst <- nrow(ordered_hospitals)
                        return(ordered_hospitals[num_worst,1])
                }else {
                        return(ordered_hospitals[num,1])
                }
                
                
        }
}