# defaults
defaultTextDirectory <- "../materials/datasets/english_split"
defaultTokensDirectory <- "./results"
defaultMatrixDirectory <- "./results"

library(quanteda)
library(readtext)


buildMatrix <- function(toks, cnt = 0, matrixDirectory = ".")
{
    cat("Computing DFM for id=", cnt, "...\n")
    dfMatr <- dfm(toks)
    cat("DFM for id=", cnt, "computed.\n")
    matrFileName <- sprintf("%s/dfm_%02d.dat", matrixDirectory, cnt)
    matrFile <- file(matrFileName, "w")
    cat("Storing DFM for id=", cnt, "in", matrFileName, "...\n")
    serialize(dfMatr, matrFile)
    close(matrFile)
    cat("DFM for id=", cnt, "stored.\n")
    
    dfMatr
}

processText <- function(textDirectory = defaultTextDirectory,
                        tokensDirectory = defaultTokensDirectory,
                        matrixDirectory = defaultMatrixDirectory,
                        buildTokens = FALSE,
                        buildMatrices = TRUE,
                        mergeMatrices = FALSE)
{
    resultMatrix <- NA
    
    if (buildTokens)
    {
        # names of files containing (split) texts
        files <- list.files(path = textDirectory, 
                            pattern = "*.txt", 
                            full.names = TRUE, 
                            recursive = FALSE)
        for (f in files)
        {
            cat("Creating corpus", cnt, "from", f, "...\n")
            partCorpus <- corpus(readtext(f, cache = FALSE, verbosity = 3))
            cat("Corpus", cnt, "created.\n")    
            cat("Reshaping corpus", cnt, "...\n")
            partCorpus <- corpus_reshape(partCorpus, to = "sentences", use_docvars = FALSE)
            cat("Corpus", cnt, "reshaped.\n")
            
            # Now, tokenize this corpus
            cat("Tokenizing corpus", cnt, "...\n")
            toks <- tokens_tolower(tokens(partCorpus, what = "word", 
                                          remove_numbers = TRUE, 
                                          remove_punct = TRUE,
                                          remove_twitter = TRUE,
                                          remove_url = TRUE,
                                          include_docvars = FALSE
            ))
            cat("Corpus", cnt, "tokenized.\n")
            
            tokFileName <- sprintf("%s/tokens_%02d.dat", tokensDirectory, cnt)
            tokFile <- file(tokFileName, "w")
            cat("Saving tokens to file ", tokFileName, "\n")
            serialize(toks, tokFile)
            close(tokFile)
            cat("Tokens for corpus", cnt, "saved.\n")
            
            
            # build DFMs if needed
            if (buildMatrices)
            {
                #dfMatr <- dfm(toks)
                #matrFileName <- sprintf("%s/dfm_%02d.dat", matrixDirectory, cnt)
                #matrFile <- file(matrFileName, "w")
                #serialize(dfMatr, matrFile)
                #close(matrFile)
                if (!is.dfm(resultMatrix))
                    resultMatrix <- buildMatrix(toks, cnt, matrixDirectory)
                else
                    resultMatrix <- rbind(resultMatrix, buildMatrix(toks, cnt, matrixDirectory))
            }
            
            cnt <- cnt + 1
        }
    }
    
    if (buildMatrices && !buildTokens)
    {
        cat("Building DFMs from saved tokens, directory", tokensDirectory, "...\n")
        
        files <- list.files(tokensDirectory, 
                            pattern = "tokens_.*.dat", 
                            full.names = TRUE,
                            recursive = FALSE)
        
        cat("Processing", length(files), "token files...\n")
        
        cnt <- 0
        for (f in files)
        {
            tokFile <- file(f, "r")
            cat("Reading tokens for id=", cnt, "from", f, "...\n")
            toks <- unserialize(tokFile)
            close(tokFile)
            cat("Tokens for id=", cnt, "read.\n")
            
            # store matrix only, do not accumulate them
            buildMatrix(toks, cnt, matrixDirectory)
            rm(toks)
            
            #newMatrix <- buildMatrix(toks, cnt, matrixDirectory)
            #rm(toks)
            #
            #if (!is.dfm(resultMatrix))
            #{
            #    #resultMatrix <- buildMatrix(toks, cnt, matrixDirectory)
            #    resultMatrix <- newMatrix
            #}
            #else
            #{
            #    #resultMatrix <- rbind(resultMatrix, buildMatrix(toks, cnt, matrixDirectory))
            #    resultMatrix <- rbind(resultMatrix, newMatrix)
            #}
            #rm(newMatrix)
            #
            #dim(resultMatrix)
            
            cnt <- cnt + 1
        }
    }    
    
    
    if (mergeMatrices)
    {
        cat("Building overall DFM from saved DFMs, directory", matrixDirectory, "...\n")
        files <- list.files(matrixDirectory, 
                            pattern = "dfm_.*.dat", 
                            full.names = TRUE,
                            recursive = FALSE)
        
        cnt <- 0
        
        for (f in files)
        {
            matrFile <- file(f, "r")
            cat("Reading DFM for id=", cnt, "from", f, "...\n")
            newMatrix <- unserialize(matrFile)
            close(matrFile)
            cat("DFM for id=", cnt, "read.\n")
            
            if (is.dfm(resultMatrix))
                resultMatrix <- rbind(resultMatrix, newMatrix)
            else
                resultMatrix <- newMatrix
            
            rm(newMatrix)
            
            cat("Iteration ", cnt, ":  dim(resultMatrix) =", dim(resultMatrix), "\n")
            cnt <- cnt + 1
        }
    }
    
    resultMatrix
}


# toksFiltered <- tokens_remove(toks, "[0-9]", valuetype = "regex", verbose=3)


# Ideas
# 1. Remove words containing numbers.
# 2. Remove profane words.
# Maybe
# 1. Remove plurals etc.
# 2. Remove words from other languages.
# 3. Remove proper names etc.
# 4. Substitute constructs like I'll, you're, we'd etc.

