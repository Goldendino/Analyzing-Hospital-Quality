## This R script creates a function called rankall that takes two arguments: an outcome name (outcome) and a hospital ranking (num). 
## The function reads the associated hospital compare database file and returns a 2-column data frame
## containing the hospital in each state that has the ranking specified in num. 

# A Note on Optimization: Instead of subsetting the data first, I have left the subsetting to respective
# if-blocks - Though the script is slightly longer, it runs significantly faster.

rankall <- function(outcome, num = "best") {
        data <- read.csv("outcome-of-care-measures.csv")
        state_column <- character()
        hospital_column <- character()
        if (outcome != "heart attack" & outcome != "heart failure" & outcome != "pneumonia" ) {
                stop("Invalid Outcome") }
        if (num == "best") {num <- 1}
        if(outcome == "heart attack") {
                relevant_data <- data[c("State", "Hospital.Name", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack")]
                list_of_dataframes <- split(relevant_data, relevant_data$State)
                for (dataframe in list_of_dataframes) {
                        dataframe$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack <- as.numeric(gsub("Not Available|Number of Cases Too Small", NA, dataframe$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack))
                        dataframe <- dataframe[!is.na(dataframe$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack), ]
                        ordered_frame <- dataframe[order(dataframe$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, dataframe$Hospital.Name), ]
                        if (num == "worst") {
                                hospital_column <- append(hospital_column, ordered_frame[nrow(ordered_frame), 2])
                        }
                        else {hospital_column <- append(hospital_column, ordered_frame[num, 2])}
                        state_column <- append(state_column, ordered_frame[1,1]) 
                }
        }
        if(outcome == "heart failure") {
                relevant_data <- data[c("State", "Hospital.Name", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure")]
                list_of_dataframes <- split(relevant_data, relevant_data$State)
                for (dataframe in list_of_dataframes) {
                        dataframe$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure <- gsub("Number of Cases Too Small", NA, dataframe$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
                        dataframe$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure <- as.numeric(gsub("Not Available|Number of Cases Too Small", NA, dataframe$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))
                        dataframe <- dataframe[!is.na(dataframe$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure), ]
                        ordered_frame <- dataframe[order(dataframe$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, dataframe$Hospital.Name), ]
                        if (num == "worst") {
                                hospital_column <- append(hospital_column, ordered_frame[nrow(ordered_frame), 2])
                        }
                        else {hospital_column <- append(hospital_column, ordered_frame[num, 2])}
                        state_column <- append(state_column, ordered_frame[1,1]) 
                }
        }
        if(outcome == "pneumonia") {
                relevant_data <- data[c("State", "Hospital.Name", "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")]
                list_of_dataframes <- split(relevant_data, relevant_data$State)
                for (dataframe in list_of_dataframes) {
                        dataframe$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia <- gsub("Number of Cases Too Small", NA, dataframe$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
                        dataframe$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia <- as.numeric(gsub("Not Available|Number of Cases Too Small", NA, dataframe$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
                        dataframe <- dataframe[!is.na(dataframe$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia), ]
                        ordered_frame <- dataframe[order(dataframe$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, dataframe$Hospital.Name), ]
                        if (num == "worst") {
                                hospital_column <- append(hospital_column, ordered_frame[nrow(ordered_frame), 2])
                        }
                        else {hospital_column <- append(hospital_column, ordered_frame[num, 2])}
                        state_column <- append(state_column, ordered_frame[1,1]) 
                }
        
        }
        completed_data_frame <- cbind.data.frame(hospital_column, state_column)
        colnames(completed_data_frame) <- c("Hospital", "State")
        return(completed_data_frame)
}