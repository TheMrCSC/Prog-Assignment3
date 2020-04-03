#Finding the best hospital in a state

best <- function(state, outcome) {
        
        #reading all the data in the csv file
        complete_data <- read.csv(".\\Data\\outcome-of-care-measures.csv", colClasses = "character")
        
        #translating outcome to column name
        #1. Splitting outcome string
        outcome_split <- strsplit(outcome, " ")[[1]]
        
        #2. taking a substring of the 1 character, Uppercasing it, 
        #       pasting it with a substring of the rest of the characters
        outcome_final <- paste(toupper(substring(outcome_split, 1,1)), substring(outcome_split, 2),
                               sep="", collapse=".")
        
        #3. getting column names
        col_name <- paste("Hospital.30.Day.Death..Mortality..Rates.from",outcome_final,sep=".")
        
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
                #if both are valid
                #calculate the min mortality for the outcome in the state
                
                #1. get min death in the state
                min_death <- min(as.numeric(complete_data[complete_data$State == state 
                                                          , col_name])
                                 ,na.rm = TRUE)
                
                #print(min_death)
                #2. create a logical list of all the cases where the deaths for 
                #    the outcome is the same as the min value, irrespective of 
                #    the state
                
                min_list <- vector(mode = "logical", length = nrow(complete_data))
                for (i in 1:nrow(complete_data)){
                        num_outcome <- as.numeric(complete_data[i,col_name])
                        if(is.na(num_outcome)){
                                min_list[i] <- FALSE
                        }else{
                                min_list[i] <- (num_outcome == min_death)
                        }
                }
                
                #print(min_list)
                
                #3. using the logical list, create a list of all the hospitals,
                #    the mortality and the states
                min_hospital_list <- complete_data[min_list,
                                                   c("Hospital.Name",col_name, "State")]
                
                #print(min_hospital_list)
                
                #4. get a list of all the hospitals from the state
                min_state_hospital_list <- min_hospital_list[min_hospital_list$State == state
                                                             , 1]
                if (length(min_state_hospital_list) == 1){
                        print(min_state_hospital_list)        
                }else{
                        print(sort(min_state_hospital_list)[1])
                }
        }
}
