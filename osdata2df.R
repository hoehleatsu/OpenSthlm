######################################################################
#' Read Open Stockholm data using JSON.
#'
#' The reader is illustrated using the 'folkmängd' as example, i.e.
#' http://data.stockholm.se/set/Befolkning/Befolkning?apikey={API-KEY}
#' where {API-KEY} denotes your API-KEY, which you obtain by registering
#' at http://dataportalen.stockholm.se/dataportalen/
#'
#' @author Michael Hoehle <http://www.math.su.se/~hoehle>
#' Date:   2015-01-23
#' License: GPL v2.0
######################################################################

library("plyr")
library("rjson")
library("dplyr")
library("ggplot2")

#' Function to load data from an URL using subsequent queries
#'
#' @param theDataURL URL of the data, see http://dataportalen.stockholm.se/dataportalen/
loadData <- function(theDataURL) {
  ##Define return variable
  df <- NULL
  ##Number of entries to skip
  skip <- 0
  ##Maximum number of records to retrieve in one GET (5000 for open data stockholm)
  top <- 5000
  ##Where to start
  stop <- FALSE

  ##Build up the data.frame by sequentially pulling the maximum number of records.
  while (!stop) {
    cat(paste0("Fetching next ",top," entries (or less).\n"))
    urlJASON  <- paste0(theDataURL,"?$top=",top,"&$skip=",skip,"&apikey=",API_KEY,"&$format=JSON&page=1")
    json_data <- rjson::fromJSON(file=urlJASON, method='C')
    ##Alternative: json_data <- jsonlite::fromJSON(txt=urlJASON)
    ##See http://stackoverflow.com/questions/16947643/getting-imported-json-data-into-a-data-frame-in-r
    oneRound <- do.call("rbind.fill", lapply(json_data, as.data.frame))
    df <- rbind(df,oneRound)
    ##Check if maximum number of records were loaded (i.e. there is more)
    stop <- (nrow(oneRound) != top)
    ##Advance countner
    skip <- skip + top
  }
  cat("Pulled",nrow(df),"entries.\n")

  return(df)
}

#' This function contains all the action. Run it and you will see
#'
main <- function() {
  #Load API_KEY stored in a text file.
  API_KEY <- scan("../API-KEY.txt",what='character')
  #Alternative - simply give the character string
  #API_KEY <- "XXXXXXYYYYYYZZZZZZZZZZZ"

  #URL of the data -- se
  url <- "http://data.stockholm.se/set/Befolkning/Befolkning"

  #Load population data for stadsdelområden
  pop <- loadData(theDataURL=url)
  dim(pop)
  head(pop, n=6)

  #=====================================================
  ### Manually massage the data. Is there a better way?
  #=====================================================

  #Convert ANTAL to actual number (not factors)
  integerNames <- c("BEF_ANTAL","YEAR")
  for (name in integerNames) {
    pop[,name] <- as.integer(as.character(pop[,name]))
  }

  #There appears to be no number ID for the AREA?
  levels(pop$AREA_TEXT)

  #Categorize age (base R version)
  pop2 <- within(pop, {
    AGE <- as.numeric(gsub("[- år| år]","",ALD11K_TEXT))
    AGEGRP <- cut(AGE, breaks=seq(0,100,by=20),include.lowest=TRUE,right=FALSE)
  })

  ##Aggregate by age-group & year.
  tab <- aggregate(BEF_ANTAL ~ AGEGRP + YEAR,sum,data=pop2)

  #===============================================
  # Be a boss at your data janitor work: dplyr
  #===============================================

  pop2 <-pop %>% mutate(AGE=as.numeric(gsub("[- år| år]","",ALD11K_TEXT)),
                        AGEGRP=cut(AGE, breaks=seq(0,100,by=20),include.lowest=TRUE,right=FALSE))
  tab <- pop2 %>% group_by(YEAR, AGEGRP) %>% summarise(total = sum(BEF_ANTAL))
  tab

  #=========================
  #Illustrate
  #=========================
  g <- ggplot(tab, aes(x=YEAR, y=total, colour=AGEGRP, group=AGEGRP)) + geom_line()
  print(g)

  #===================================================================================
  #Convert data.frame to matrix (representing a time series) - useful for ts analysis.
  #===================================================================================
  ts <- xtabs(total ~ YEAR + AGEGRP, data=tab)

  return(ts)
}

#Do it! Or go to main function and execute contents line-by-line
main()

