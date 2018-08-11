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

# Create many corpora, analyze them and remove
files <- list.files(path = "../materials/datasets/english_split", 
                    pattern = "*.txt", 
                    full.names = TRUE, 
                    recursive = FALSE)

cnt <- 0

for (f in files)
{
    cat("Creating corpus", cnt, "from", f, "...\n")
    partCorpus <- corpus(readtext(f, cache = FALSE, verbosity = 3))
    cat("Corpus", cnt, "created.\n")    
    cat("Reshaping corpus", cnt, "...\n")
    partCorpus <- corpus_reshape(partCorpus, to = "sentences", use_docvars = FALSE)
    cat("Corpus", cnt, "reshaped.\n")
    
    cnt <- cnt + 1
}
