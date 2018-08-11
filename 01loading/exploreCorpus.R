library(quanteda)
library(readtext)

##enCorpus <- corpus(readtext("../materials/datasets/final/en_US/*.txt",
##                     cache = FALSE,
##                     verbosity = 3))
#
#enCorpus <- corpus(readtext("../materials/datasets/english_split/*.txt",
#                            cache = FALSE,
#                            verbosity = 3))

#enCorpus <- corpus_reshape(enCorpus, to = "sentences", use_docvars = FALSE)

tokensDir <- "results"

# Create many corpora, analyze them and remove
files <- list.files(path = "../materials/datasets/english_split", 
                    pattern = "*.txt", 
                    full.names = TRUE, 
                    recursive = FALSE)

cnt <- 0

#for (f in files)
for (f in files[1])
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
    
    tokFileName <- sprintf("%s/tokens_%02d.dat", tokensDir, cnt)
    tokFile <- file(tokFileName, "w")
    cat("Saving tokens to file ", tokFileName, "\n")
    serialize(toks, tokFile)
    close(tokFile)
    cat("Tokens for corpus", cnt, "saved.\n")
    
    cnt <- cnt + 1
}
