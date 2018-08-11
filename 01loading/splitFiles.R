files <- list.files(path = "../materials/datasets/final/en_US", 
                    pattern = "*.txt", 
                    full.names = TRUE, 
                    recursive = FALSE)

maxLines <- 500000
outputDir <- "../materials/datasets/english_split"

cnt <- 0

for (f in files)
{
    inputCon <- file(f, "r")
    
    fileProcessed <- FALSE
    splitCnt <- 0
    
    while (!fileProcessed)
    {
        cat("Reading", maxLines, "from", f, "...\n")
        lines <- readLines(inputCon, maxLines)
        cat("Lines read.")
    
        if (length(lines) > 0)
        {
            outputPathname <- sprintf("%s/%d_%d.txt", outputDir, cnt, splitCnt)
            cat("Writing", length(lines), "to", outputPathname, "\n")
            writeLines(lines, outputPathname)
            cat(length(lines), "written to", outputPathname, "\n")
            splitCnt <- splitCnt + 1    
        }
        else
            fileProcessed <- TRUE
    }
    
    close(inputCon)
    
    cnt <- cnt + 1
}
