rankhospital <- function(state, outcome, num = "best") { 
  ## Read outcome data
  ## Check that state and outcome are valid
  ##### rankhospital("NY", "heart attak", 7)
  ## Return hospital name in that state with the given rank ## 30-day death rate
	O <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
	O[,c(1,2,7,11,17,23)] -> H.30.DMR
	for (i in 4:6) {
					H.30.DMR[,i][H.30.DMR[,i] == "Not Available"] <- NA
					as.numeric(H.30.DMR[,i]) -> H.30.DMR[,i]
					}
	c("heart attack","heart failure","pneumonia") -> names(H.30.DMR)[4:6] -> Oc
	unique(H.30.DMR$State) -> St
	if (is.na(match(state,St))) stop("invalid state")
	if (is.na(match(outcome,Oc))) stop("invalid outcome")
	subset(H.30.DMR, State == state , select=c("Provider.Number","Hospital.Name","State",outcome)) -> X
	X[complete.cases(X),] -> X
	X[order(X[outcome],X$Hospital.Name),c("Hospital.Name")] -> X
	if (num=="best") return(X[1])
	if (num=="worst") return(X[length(X)])
	if (is.numeric(num)) return(X[num])
}