### Useful funtion to save time

## getting data,
## type == choice of climate water or well
## data == actual file.

get_data <- function(type,data){
  path <- file.path("C:", "Users", "dustin.smith", "OneDrive - Lubbock Independent School District",
                    "Documents","Codes","R","Climate data and Water Levels","Original Data Sets",fsep = "\\")
  type <- file.path("Climate Data",fsep = "\\")
  data <- read.csv(paste(path,type,"San Antonio Airport (SAT) climate data (1948 to 2025).csv",sep = "\\"))

  return(data)
}
