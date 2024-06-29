## This R script creates a function "best" with the capability to search the hospital compare database 
## by state (e.g. Texas) and one of three outcomes (mortality rate of pneumonia, heart attack, 
## or heart failure) - It returns the hospital in the specified state with the lowest mortality rate of
## the specified outcome.

# A Note on Optimization: Instead of subsetting the data first, I have left the subsetting to respective
# if-blocks - Though the code is slightly longer, it runs significantly faster.


best <- function(state, outcome) {
        data <- read.csv("outcome-of-care-measures.csv")
        if(!state %in% data$State) {stop("Invalid State")}
        if (outcome != "heart attack" & outcome != "heart failure" & outcome != "pneumonia" ) {
                stop("Invalid Outcome")
        }
        if(outcome == "heart attack") {
                extracted_data <- data[c("Hospital.Name", "State", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack")]
                state_hospital_data <- extracted_data[extracted_data$State == state, ]
                state_hospital_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack<- as.numeric(state_hospital_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
                minimum <- min(state_hospital_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, na.rm = TRUE) 
                final_data <- state_hospital_data[state_hospital_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack == minimum,]
                ordered_final_data <- final_data[order(final_data$Hospital.Name), ]
                return(ordered_final_data[1,1])
                
        }
        else if (outcome == "heart failure") {
                extracted_data <- data[c("Hospital.Name", "State", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure")]
                state_hospital_data <- extracted_data[extracted_data$State == state, ]
                state_hospital_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure <- gsub("Not Available", NA, state_hospital_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
                state_hospital_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure <- gsub("Number of Cases Too Small", NA, state_hospital_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
                state_hospital_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure<- as.numeric(state_hospital_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
                minimum <- min(state_hospital_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, na.rm = TRUE)
                final_data <- state_hospital_data[state_hospital_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure == minimum,]
                ordered_final_data <- final_data[order(final_data$Hospital.Name), ]
                return(ordered_final_data[1,1])
        }
        else if (outcome == "pneumonia") {
                extracted_data <- data[c("Hospital.Name", "State", "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")]
                state_hospital_data <- extracted_data[extracted_data$State == state, ]
                state_hospital_data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia <- gsub("Not Available", NA, state_hospital_data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
                state_hospital_data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia<- as.numeric(state_hospital_data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
                minimum <- min(state_hospital_data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, na.rm = TRUE) 
                final_data <- state_hospital_data[state_hospital_data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia == minimum,]
                ordered_final_data <- final_data[order(final_data$Hospital.Name), ]
                return(ordered_final_data[1,1])
        }
       
}