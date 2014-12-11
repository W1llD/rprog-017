rankall <- function(outcome, num = "best") { 
  ## Read outcome data
  ## Check that state and outcome are valid
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the ## (abbreviated) state name
  O <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  O[,c(1,2,7,11,17,23)] -> H.30.DMR
  for (i in 4:6) {
    H.30.DMR[,i][H.30.DMR[,i] == "Not Available"] <- NA
    as.numeric(H.30.DMR[,i]) -> H.30.DMR[,i]
  }
  c("heart attack","heart failure","pneumonia") -> names(H.30.DMR)[4:6] -> Oc
  sort(unique(H.30.DMR$State)) -> St
  if (is.na(match(outcome,Oc))) stop("invalid outcome")
  ##process
  subset(H.30.DMR, !is.na(H.30.DMR[outcome]), select=c("Hospital.Name","State",outcome)) -> X
  ranks <- function(state) {
    X[X$State == state,] -> sX
    sX[order(sX[outcome],sX["Hospital.Name"]),c("Hospital.Name")] -> sX
    if (num=="best") return(data.frame(hospital = sX[1], state = state))
    if (num=="worst") return(data.frame(hospital = sX[length(sX)], state = state))
    if (is.numeric(num)) return(data.frame(hospital = sX[num], state = state)) 
  }
  data.frame(hospital = character(0), state = character(0)) -> r
  for (s in St) {rbind(r,ranks(s)) -> r}
  names(r) <- c("hospital","state")
  return(r)
}
