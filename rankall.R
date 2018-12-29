rankall <- function(outcome, num = "best") {
        numb <- num
        ## Read outcome data
        print(numb)
        setwd(
                "G:/My Drive/Coursera Data Science Specialization/rprog_data_ProgAssignment3-data/"
        )
        data <-
                read.csv("outcome-of-care-measures.csv",
                         stringsAsFactors = FALSE,
                         na.strings = c("Not Available")
                         )
        ## Check that state and outcome are valid
        if (tolower(outcome) == "heart attack") {
                col_index <- 11
        } else if (tolower(outcome) == "heart failure") {
                col_index <- 17
        } else if (tolower(outcome) == "pneumonia") {
                col_index <- 23
        } else {
                stop("invalid outcome")
        }
        if (numb == "best") {
                numb <- 1
        }
        output <- data.frame(hospital = character(), state = character())
        ## For each state, find the hospital of the given rank
        states_used <- unique(data[, 7]) #54. The base 50 plus guam, puerto rico
                #virgin islands, and DC
        states <- states_used[order(states_used)]
        for (state1 in states) { #iterate through the states
                row_TF <- data[, 7] == state1  
                        #select the data for the correct state
                new_data <- data[row_TF, ] #limit to the correct state's data
                suppressWarnings(
                        num_rows <- length(na.omit(as.numeric(new_data[, col_index])))
                                )
                new_data_TF <- !is.na(new_data[, col_index])
                new_data <- new_data[new_data_TF,]
                suppressWarnings(values <-
                                         data.frame(
                                                 new_data[, 2], 
                                                 new_data[, 7], 
                                                 new_data[, col_index],
                                                 stringsAsFactors = FALSE
                                                 )
                                )
                                #collect the important data
                order_for_values_index <- order(
                                                values[, 3], #the metric value
                                                values[, 1], #the hospital name
                                                decreasing = FALSE
                                                )
                values <- values[order_for_values_index,]
                        #get the appropriate indexing to sort by rate and then by
                        #name both in ascending order
                if (num == "worst") {
                        numb <- num_rows
                }
                if (numb > num_rows) {
                        name <- NA
                }
                name <- values[numb, 1]
                output <-
                        rbind(output,
                              data.frame(hospital = name, state = state1))
                ## Return a data frame with the hospital names and the
                ## (abbreviated) state name
        }
        output
}

# > source("rankall.R")
head(rankall("heart attack", 20), 10)
# hospital state
# AK <NA> AK
# AL D W MCMILLAN MEMORIAL HOSPITAL AL
# AR ARKANSAS METHODIST MEDICAL CENTER AR
# AZ JOHN C LINCOLN DEER VALLEY HOSPITAL AZ
# CA SHERMAN OAKS HOSPITAL CA
# CO SKY RIDGE MEDICAL CENTER CO
# CT MIDSTATE MEDICAL CENTER CT
# DC <NA> DC
# DE <NA> DE
# FL SOUTH FLORIDA BAPTIST HOSPITAL FL
tail(rankall("pneumonia", "worst"), 3)
        #Only way to make this work is to use
                #numb <- num at the beginning
                #if (num == "worst") { numb <- num_rows } in the middle
# hospital state
# WI MAYO CLINIC HEALTH SYSTEM - NORTHLAND, INC WI
# WV PLATEAU MEDICAL CENTER WV
# WY NORTH BIG HORN HOSPITAL DISTRICT WY
tail(rankall("heart failure"), 10)
# hospital state
# TN WELLMONT HAWKINS COUNTY MEMORIAL HOSPITAL TN
# TX FORT DUNCAN MEDICAL CENTER TX
# UT VA SALT LAKE CITY HEALTHCARE - GEORGE E. WAHLEN VA MEDICAL CENTER UT
# VA SENTARA POTOMAC HOSPITAL VA
# VI GOV JUAN F LUIS HOSPITAL & MEDICAL CTR VI
# VT SPRINGFIELD HOSPITAL VT
# WA HARBORVIEW MEDICAL CENTER WA
# WI AURORA ST LUKES MEDICAL CENTER WI
# WV FAIRMONT GENERAL HOSPITAL WV
# WY CHEYENNE VA MEDICAL CENTER WY