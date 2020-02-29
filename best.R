best <- function(state, outcome) {
        ## Read outcome data
        ## Check that state and outcome are valid
        ## Return hospital name in that state with lowest 30-day death
        ## rate
        
        # initialize valid outcome values
        Voutcomes <- c("heart attack","heart failure","pneumonia")
        Coloutcomes<-c(11,17,23)
        names(Coloutcomes)<-Voutcomes
        
        # check outcome argument
        if (isFALSE(outcome %in% Voutcomes)) {
                stop("invalid outcome!!")  
        }
        
        # read the data
        outcome.data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        # generate the vector of the states
        Vstates<-unique(outcome.data[,7])
        
        # check state argument
        if (isFALSE(state %in% Vstates)) {
              stop("invalid state!!")  
        }
        # create a sub data frame from state and outcome
        suboutcome.data<-outcome.data[outcome.data$State==state,c(2,Coloutcomes[outcome])]
        
        # convert the outcome value
        suboutcome.data[,2]=suppressWarnings(as.numeric(suboutcome.data[,2]))
        
        # order data by the outcome frame (descending) and by
        # the hospital name (ascending)
        outcomeSorted<-order(suboutcome.data[,2],
                             suboutcome.data[,1],
                             method="radix",
                             na.last = T)
        # print the best Hospital for the given outcome and state
        suboutcome.data[outcomeSorted[1],1]

}