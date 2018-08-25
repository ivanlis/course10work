library(data.table)

bigramsToSuggest = 10
trigramsToSuggest = 10

loadDatabase <- function(ngramPath = "../results/tables")
{
    tableWord <- fread(sprintf("%s/tableWord.csv", ngramPath))
    setkey(tableWord, word)
    tableBigram <- fread(sprintf("%s/tableBigram.csv", ngramPath))
    setkey(tableBigram, id1, id2)
    tableTrigram <- fread(sprintf("%s/tableTrigram.csv", ngramPath))
    setkey(tableTrigram, id1, id2, id3)
    
    list(unigram = tableWord, bigram = tableBigram, trigram = tableTrigram)
}

predictWord <- function(database, string)
{
    words <- tail(strsplit(tolower(string), "\\s+", fixed = FALSE, perl = TRUE)[[1]], 2)
    
    
    bigramResult <- character(0)
    trigramResult <- character(0)
    
    if (length(words) >= 1)
    {
        searchWord1 <- words[length(words)]
        
        bigrams <- ((merge(database$unigram[word == searchWord1, .(id, word)], 
                         database$bigram, by.x = "id", by.y = "id1")
                    [,.(id2, probability)])[order(-probability)])[1:min(bigramsToSuggest, .N)]
        bigramResult <- database$unigram[id %in% bigrams$id2, word]
        
        if (length(words) >= 2)
        {
            searchWord2 <- words[length(words) - 1]
            
            trigrams <- ((merge(merge(database$unigram[word == searchWord2, .(id, word)], 
                                      database$trigram, by.x = "id", by.y = "id1")
                                [, .(id2, id3, probability)],
                                
                                database$unigram[word == searchWord1, .(id, word)],
                                by.x = "id2", by.y = "id")
                          [,.(id3, probability)])[order(-probability)])[1:min(trigramsToSuggest, .N)]            
            trigramResult <- database$unigram[id %in% trigrams$id3, word]
        }
    }
    
    list(bigramWords = bigramResult, trigramWords = trigramResult)
}
