maxLineLength <- function()
{
    fileNames <- c("../materials/datasets/final/en_US/en_US.blogs.txt",
                   "../materials/datasets/final/en_US/en_US.news.txt",
                   "../materials/datasets/final/en_US/en_US.twitter.txt")
    
    maxLength <- vector("numeric", length(fileNames))
    minLength <- vector("numeric", length(fileNames))
    
    maxLines <- 250000
    
    fileCnt <- 1
    for (f in fileNames)
    {
        cat("Processing", f, "...\n")
        
        maxLength[fileCnt] <- -Inf
        minLength[fileCnt] <- Inf
        
        con <- file(f, "r")
        fileProcessed <- FALSE        
        
        while (!fileProcessed)
        {
            lines <- readLines(con, n = maxLines)
            
            if (length(lines) > 0)
            {
                linesLength <- sapply(lines, FUN = function(str) { nchar(str) }, USE.NAMES = FALSE)
                currMin <- min(linesLength)
                currMax <- max(linesLength)
                cat("Current range:", currMin, "..", currMax, "\n")
                #cat("Comparing", maxLength[fileCnt], "<", currMax, "...\n")
                if (maxLength[fileCnt] < currMax)
                    maxLength[fileCnt] <- currMax
                if (minLength[fileCnt] > currMin)
                    minLength[fileCnt] <- currMin
            }
            else
                fileProcessed = TRUE
        }
        close(con)
        
        fileCnt <- fileCnt + 1
    }
    
    list(minLength, maxLength)
}
