best <- function(state, outcome) {
## Read outcome data
## Check that state and outcome are valid
## Return hospital name in that state with lowest 30-day death rate
## outcome in “heart attack”, “heart failure”, or “pneumonia”. 
	O <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
	O[,c(1,2,7,11,17,23)] -> H.30.DMR
	for (i in 4:6) {
					H.30.DMR[,i][H.30.DMR[,i] == "Not Available"] <- NA
					as.numeric(H.30.DMR[,i]) -> H.30.DMR[,i]
					}
	c("heart attack","heart failure","pneumonia") -> names(H.30.DMR)[4:6] -> Oc
	unique(H.30.DMR$State) -> St
	if (is.na(match(state,St))) stop("invalid state")
	if (is.na(match(outcome,Oc))) stop("invalid outcom")
	min(H.30.DMR[,outcome][H.30.DMR[,3] == state],na.rm = T) -> lowest 
	sort(H.30.DMR[,2][H.30.DMR[3] == state & H.30.DMR[outcome] == lowest & !is.na(H.30.DMR[outcome])]) -> best
	return(best)
}