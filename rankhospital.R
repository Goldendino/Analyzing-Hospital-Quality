## rankhospital that takes three arguments: the 2-character abbreviated name of a
## state (state), an outcome (outcome), and the ranking of a hospital in that state for that outcome (num).
## The function reads the outcome-of-care-measures.csv file and returns a character vector with the name
## of the hospital that has the ranking specified by the num argument.

# A Note on Optimization: Instead of subsetting the data first, I have left the subsetting to respective
# if-blocks - Though the script is slightly longer, it runs significantly faster.

rankhospital <- function(state, outcome, num = "best") {
        data <- read.csv("outcome-of-care-measures.csv")
        if(!state %in% data$State) {stop("Invalid State")}
        if (outcome != "heart attack" & outcome != "heart failure" & outcome != "pneumonia" ) {
                stop("Invalid Outcome") }
        if(outcome == "heart attack") {
                extracted_data <- data[c("State", "Hospital.Name", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack")]
                state_hospital_data <- extracted_data[extracted_data$State == state, ]
                state_hospital_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack <- gsub("Not Available", NA, state_hospital_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
                state_hospital_data <- na.omit(state_hospital_data)
                state_hospital_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack <- as.numeric(state_hospital_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
                ordered_data <- state_hospital_data[order(state_hospital_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, state_hospital_data$Hospital.Name), ]
                ordered_data$Rank <- 1:nrow(ordered_data)
                final_data <- ordered_data[, 2:4]
                colnames(final_data) <- c("Hospital.Name", "Rate", "Rank")
                }
        else if (outcome == "heart failure") {
                extracted_data <- data[c("State", "Hospital.Name", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure")]
                state_hospital_data <- extracted_data[extracted_data$State == state, ]
                state_hospital_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure <- gsub("Not Available", NA, state_hospital_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
                state_hospital_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure <- gsub("Number of Cases Too Small", NA, state_hospital_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
                state_hospital_data <- na.omit(state_hospital_data)
                state_hospital_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure <- as.numeric(state_hospital_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
                ordered_data <- state_hospital_data[order(state_hospital_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, state_hospital_data$Hospital.Name), ]
                ordered_data$Rank <- 1:nrow(ordered_data)
                final_data <- ordered_data[, 2:4] 
                colnames(final_data) <- c("Hospital.Name", "Rate", "Rank") 
                }
        else if (outcome == "pneumonia") {
                extracted_data <- data[c("State", "Hospital.Name", "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")]
                state_hospital_data <- extracted_data[extracted_data$State == state, ]
                state_hospital_data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia <- gsub("Not Available", NA, state_hospital_data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
                state_hospital_data <- na.omit(state_hospital_data)
                state_hospital_data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia <- as.numeric(state_hospital_data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
                ordered_data <- state_hospital_data[order(state_hospital_data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, state_hospital_data$Hospital.Name), ]
                ordered_data$Rank <- 1:nrow(ordered_data)
                final_data <- ordered_data[, 2:4]
                colnames(final_data) <- c("Hospital.Name", "Rate", "Rank")
        }
        
        if (num =="best") {return(final_data[1,1])}
        if (num =="worst") {return(final_data[nrow(final_data),1])}
        if (is.numeric(num)) {return(final_data[num,1])}
}