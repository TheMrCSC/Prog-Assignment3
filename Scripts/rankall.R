rankall <- function(outcome, num = "best") {

        ## For each state, find the hospital of the given rank
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
        
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
        
        #checking validity of outcome
        #checking if the columname derived exists in names of the dataset
        invalid_outcome <- !(col_name %in% names(complete_data))
        
        if(invalid_outcome){
                #if outcome is invalid stop the process
                stop("invalid outcome")
                
        }else{
                #selecting required subset of data
                hospital_state_outcome <- complete_data[
                                complete_data[col_name] != "Not Available",
                        c("State", col_name , "Hospital.Name")]
                
                #updating the oucome column name to Rate
                colnames(hospital_state_outcome)[2] <- "Rate"
                
                #converting Rate to numeric from character
                hospital_state_outcome$Rate <- as.numeric(as.character(
                        hospital_state_outcome$Rate))
                
                #getting a datasets of all states
                states <- unique(complete_data["State"])
                all_states <- data.frame(states[order(states$State),"State"])
                colnames(all_states) <- "State"
                
                #merging all_states to houspital_state_outcome to ensure that
                #all states are present
                
                all_states_hospitals <- merge(x = all_states, 
                                              y = hospital_state_outcome,
                                              by.x = "State", by.y = "State",
                                              all.x = TRUE)
                
                #ordering the hospitals according to the state, rate and 
                #hospital name
                ordered_hospitals <- all_states_hospitals[order(
                        all_states_hospitals$State
                        ,all_states_hospitals$Rate
                        ,all_states_hospitals$Hospital.Name),]
                
                #splitting the dataset on the basis of the state
                ind_state <- split(ordered_hospitals,ordered_hospitals$State)
                
                #create output dataframe
                output <- data.frame(all_states$State, 
                                     row.names = all_states$State)
                colnames(output) <- c("state")
                output$hospital <- "NA"
                
                #change "best" to 1
                num_int <- 0
                if (num == "best") {num_int <- 1} else {num_int <- num}
                
                for ( i in as.vector(all_states$State)){
                        ind_state_temp <- ind_state[[i]]
                        if (num == "worst") {num_int <- nrow(ind_state_temp)}
                        output[i,2]<-as.character(ind_state_temp[
                                num_int,c("Hospital.Name")])
                }
                output
        }
}