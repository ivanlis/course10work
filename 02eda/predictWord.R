library(data.table)

bigramsToSuggest = 10
trigramsToSuggest = 10
fourgramsToSuggest = 10

loadDatabase <- function(ngramPath = "../results/tables")
{
    tableWord <- fread(sprintf("%s/tableWord.csv", ngramPath))
    setkey(tableWord, word)
    tableBigram <- fread(sprintf("%s/tableBigram.csv", ngramPath))
    setkey(tableBigram, id1)
    tableTrigram <- fread(sprintf("%s/tableTrigram.csv", ngramPath))
    setkey(tableTrigram, id1, id2)
    tableFourgram <- fread(sprintf("%s/tableFourgram.csv", ngramPath))
    setkey(tableFourgram, id1, id2, id3)
    
    list(unigram = tableWord, bigram = tableBigram, 
         trigram = tableTrigram, fourgram = tableFourgram)
}

predictWord <- function(database, string)
{
    words <- tail(strsplit(tolower(string), "\\s+", fixed = FALSE, perl = TRUE)[[1]], 3)
    
    
    bigramResult <- character(0)
    trigramResult <- character(0)
    fourgramResult <- character(0)
    
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
            
            
            if (length(words) >= 3)
            {
                searchWord3 <- words[length(words) - 2]
                #cat("searchWord3", searchWord3, "\n")
                
                fourgrams <- ((merge(merge(merge(database$unigram[word == searchWord3, .(id, word)],
                                   database$fourgram, by.x = "id", by.y = "id1")
                              [, .(id2, id3, id4, probability)],
                              
                              database$unigram[word == searchWord2, .(id, word)],
                              by.x = "id2", by.y = "id")[, .(id3, id4, probability)],
                              database$unigram[word == searchWord1, .(id, word)],
                              by.x = "id3", by.y = "id")[, .(id4, probability)])
                            [order(-probability)])[1:min(fourgramsToSuggest, .N)]
                              
                fourgramResult <- database$unigram[id %in% fourgrams$id4, word]
            }
        }
    }
    
    list(bigramWords = bigramResult, trigramWords = trigramResult, 
         fourgramWords = fourgramResult)
}
