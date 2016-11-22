###############################################################################
###############################################################################
###############################################################################

## loaduju balíčky ------------------------------------------------------------

library(openxlsx)


## ----------------------------------------------------------------------------

###############################################################################

## nastavuji pracovní složku --------------------------------------------------

setwd(choose.dir())

mother_working_directory <- getwd()


## ----------------------------------------------------------------------------

###############################################################################

## vytvářím složku pro výsledné diagramy --------------------------------------

if(!file.exists("diagramy_nad_vysledky_uchazecu")){
    dir.create(file.path(
    mother_working_directory, "diagramy_nad_vysledky_uchazecu"
))
}


## ----------------------------------------------------------------------------
## ----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################

## helper funkce --------------------------------------------------------------

  ## funkce pro získání bodových zisků ----------------------------------------
  
  getMyScores <- function(data){
    
    my_scores <- NULL
    
    for(i in 1:dim(data)[1]){
      my_scores <- c(my_scores,
                     sum(
                       grepl("X",
                             data[i,
                                  which(grepl(pattern = "[0-9]+",
                                              x = colnames(data)))]
                       )
                     )
      )
    }
    
    return(my_scores)
    
  }


## ----------------------------------------------------------------------------

## funkce pro vytvoření nula-jedničkovou transformaci -------------------------
  
  getMyTrueFalseTable <- function(data){
    
    if(sum(grepl("id", colnames(data))) > 0){
      
      temp_data <- as.data.frame(
        data[, which(grepl("id", colnames(data)))[1]]
        )
      
    }else{
      
      temp_data <- data.frame("id" = as.factor(rep(NA, dim(data)[1])))
      
    }
    
    for(i in which(grepl(pattern = "[0-9]+", x = colnames(data)))){
      
      temp_data <- as.data.frame(
        cbind(temp_data, grepl(pattern = "X", x = data[, i]))
      )
      
    }
    
    colnames(temp_data) <- c(
      "id",
      colnames(data)[which(grepl(pattern = "[0-9]+", x = colnames(data)))]
    )
    
    return(temp_data)
    
  }


## ----------------------------------------------------------------------------

## funkce pro vykreslení histogramu nad bodovými zisky v rámci předmětu -------
  
  getMyHistogram <- function(my_data, number_of_breaks = 5){

    ###########################################################################
    
    my_scores <- getMyScores(my_data)
    
    
    ###########################################################################
    
    ## vykresluji histogram ---------------------------------------------------
    
    hist(
      my_scores,
      breaks = number_of_breaks,
      xlim = c(0, length(
        which(
          grepl(pattern = "[0-9]+", x = colnames(my_data))
        )
      )),
      main = "Histogram bodových zisků uchazečů \nvšeobecné lékařství",
      xlab = "hodnota bodového zisku",
      ylab = "absolutní počet studentů",
      col = "lightgrey"
    )
    
  }


## ----------------------------------------------------------------------------

## funkce pro vykreslení obtížnost-diskriminace diagramu ----------------------
  
  getMyDifficultyDiscriminationPlot <- function(my_data){    

    ###########################################################################
    
    ## doluju bodová skóre pro všechny uchazeče
    
    my_scores <- getMyScores(my_data)
    
    
    ###########################################################################
    
    ## vytvářím nula-jedničkovou tranformaci dle správnosti odpovědí,
    ## nadále už nebudou původní odpovědi uhcazečů třeba
    
    my_data <- getMyTrueFalseTable(my_data)
    
    
    ###########################################################################
    
    ## vytvářím kvintily
    
    my_quintiles <- NULL
    
    for(i in 0:5){
      my_quintiles <- c(my_quintiles,
                        quantile(my_scores, probs = 0.2*i, names = FALSE)
      )
    }
    
    my_quintiles[c(1, 6)] <- c(0, (dim(my_data)[2] - 1) + 1)
    my_quintiles <- ceiling(my_quintiles)
    
    
    ###########################################################################
    
    ## vytvářím bary
    
    ## obtížnost otázky chápu jako 1 - podíl správně odpovídajících studentů
    ## ku všem studentům odpovídajícím na otázku
    
    obtiznosti <- NULL
    
    for(i in 2:dim(my_data)[2]){
      obtiznosti <- c(obtiznosti,
                      1 - length(which(my_data[, i]))/length(my_data[, i])
      )
    }
    
    
    ###########################################################################
    
    ## diskriminační schopnost otázky chápu jako podíl správně odpovídajících
    ## studentů ku všem studentům odpovídajícím na otázku určitého kvantilu,
    ## to celé lomenu podílem správně odpovídajících studentů ku všem
    ## studentům odpovídajícím na otázku určitého jiného kvantilu
    
    ## diskriminace jako 5. kvintil - 4. kvintil
    ## i jako 5. kvintil - 1. kvintil
    
    
    ###########################################################################
    
    diskriminace_nizsi <- NULL
    
    for(i in 2:dim(my_data)[2]){
      diskriminace_nizsi <- c(
        diskriminace_nizsi,
        ((sum(
          my_data[which(my_scores >= my_quintiles[5]), i]
        ) / length(
          my_data[which(my_scores >= my_quintiles[5]), i]
        )) - (sum(
          my_data[which(my_scores < my_quintiles[2]), i]
        ) / length(
          my_data[which(my_scores < my_quintiles[2]), i]
        )))
      )
    }
    
    
    ###########################################################################
    
    diskriminace_vyssi <- NULL
    
    for(i in 2:dim(my_data)[2]){
      diskriminace_vyssi <- c(
        diskriminace_vyssi,
        ((sum(
          my_data[which(my_scores >= my_quintiles[5]), i]
        ) / length(
          my_data[which(my_scores >= my_quintiles[5]), i]
        )) - (sum(
          my_data[which(my_scores < my_quintiles[5] &
                       my_scores >= my_quintiles[4]), i]
        ) / length(
          my_data[which(my_scores < my_quintiles[5] &
                       my_scores >= my_quintiles[4]), i]
        )))
      )
    }
    
    
    ###########################################################################
    
    ## koriguji velmi záporné hodnoty diskriminačních měr, to kvůli
    ## komfortnějšímu vykreslení v diagramu, aby záporné sloupečky
    ## nezasahovaly do popisků položek
    
    diskriminace_nizsi[which(diskriminace_nizsi < 0)] <- -0.01
    diskriminace_vyssi[which(diskriminace_vyssi < 0)] <- -0.01
    
    
    ###########################################################################
    
    ## určuji soubor k vykreslení
    
      my_sample <- rbind("obtiznost" = obtiznosti[order(obtiznosti)],
                         "5p_1p" = diskriminace_nizsi[order(obtiznosti)],
                         "5p_4p" = diskriminace_vyssi[order(obtiznosti)]
      )
      my_colours <- c("red", "darkgrey", "blue")   
    
    
    ###########################################################################
    
    ## vykresluji konečný barplot
    
    barplot(
      my_sample,
      beside = TRUE,
      col = my_colours,
      xlab = "",
      ylim = c(0.0, 1.0),
      main = "Diagram obtížnosti vs. diskriminace \nvšeobecné lékařství",
      horiz = FALSE
    )
    
    title(xlab = "číslo otázky (řazeno dle obtížnosti)",
          line = 4)
    
            abline(h = c(0.2, 0.4), lty = 2)
            
            axis(side = 1,
                 at = seq(2.5, 2.5 + 4 * (dim(my_data)[2] - 2), by = 4),
                 labels = colnames(my_data)[
                   2:dim(my_data)[2]
                   ][order(obtiznosti)],
                 las = 2
            )
            
            legend(x = "topleft",
                   inset = c(0.04, 0.0),
                   legend = c(
                     "obtížnost",
                     "diskriminace (rozdíl podílu úspěšných 5. a 1. pětiny)",
                     "diskriminace (rozdíl podílu úspěšných 5. a 4. pětiny)"
                   ),
                   pch = 19,
                   col = my_colours,
                   bg = "white",
                   title = expression(bold("legenda")))
    
  }



## ----------------------------------------------------------------------------

## printable verze 'obtížnost-diskriminace' diagramu --------------------------
  
  getMyFirstHalfDifficultyDiscriminationPlot <- function(my_data){
    
    ###########################################################################
    
    ## doluju bodová skóre pro všechny uchazeče
    
    my_scores <- getMyScores(my_data)
    
    
    ###########################################################################
    
    ## vytvářím nula-jedničkovou tranformaci dle správnosti odpovědí,
    ## nadále už nebudou původní odpovědi uhcazečů třeba
    
    my_data <- getMyTrueFalseTable(my_data)
    
    
    ###########################################################################
    
    ## vytvářím kvintily
    
    my_quintiles <- NULL
    
    for(i in 0:5){
      my_quintiles <- c(my_quintiles,
                        quantile(my_scores, probs = 0.2*i, names = FALSE)
      )
    }
    
    my_quintiles[c(1, 6)] <- c(0, (dim(my_data)[2] - 1) + 1)
    my_quintiles <- ceiling(my_quintiles)
    
    
    ###########################################################################
    
    ## vytvářím bary
    
    ## obtížnost otázky chápu jako 1 - podíl správně odpovídajících studentů
    ## ku všem studentům odpovídajícím na otázku
    
    obtiznosti <- NULL
    
    for(i in 2:dim(my_data)[2]){
      obtiznosti <- c(obtiznosti,
                      1 - length(which(my_data[, i]))/length(my_data[, i])
      )
    }
    
    
    ###########################################################################
    
    ## diskriminační schopnost otázky chápu jako podíl správně odpovídajících
    ## studentů ku všem studentům odpovídajícím na otázku určitého kvantilu,
    ## to celé lomenu podílem správně odpovídajících studentů ku všem
    ## studentům odpovídajícím na otázku určitého jiného kvantilu
    
    ## diskriminace jako 5. kvintil - 4. kvintil
    ## i jako 5. kvintil - 1. kvintil
    
    
    ###########################################################################
    
    diskriminace_nizsi <- NULL
    
    for(i in 2:dim(my_data)[2]){
      diskriminace_nizsi <- c(
        diskriminace_nizsi,
        ((sum(
          my_data[which(my_scores >= my_quintiles[5]), i]
        ) / length(
          my_data[which(my_scores >= my_quintiles[5]), i]
        )) - (sum(
          my_data[which(my_scores < my_quintiles[2]), i]
        ) / length(
          my_data[which(my_scores < my_quintiles[2]), i]
        )))
      )
    }
    
    
    ###########################################################################
    
    diskriminace_vyssi <- NULL
    
    for(i in 2:dim(my_data)[2]){
      diskriminace_vyssi <- c(
        diskriminace_vyssi,
        ((sum(
          my_data[which(my_scores >= my_quintiles[5]), i]
        ) / length(
          my_data[which(my_scores >= my_quintiles[5]), i]
        )) - (sum(
          my_data[which(my_scores < my_quintiles[5] &
                          my_scores >= my_quintiles[4]), i]
        ) / length(
          my_data[which(my_scores < my_quintiles[5] &
                          my_scores >= my_quintiles[4]), i]
        )))
      )
    }
    
    
    ###########################################################################
    
    ## koriguji velmi záporné hodnoty diskriminačních měr, to kvůli
    ## komfortnějšímu vykreslení v diagramu, aby záporné sloupečky
    ## nezasahovaly do popisků položek
    
    diskriminace_nizsi[which(diskriminace_nizsi < 0)] <- -0.01
    diskriminace_vyssi[which(diskriminace_vyssi < 0)] <- -0.01
    
    
    ###########################################################################
    
    ## určuji soubor k vykreslení
    
      my_sample <- rbind("obtiznost" = obtiznosti[order(obtiznosti)],
                         "5p_1p" = diskriminace_nizsi[order(obtiznosti)],
                         "5p_4p" = diskriminace_vyssi[order(obtiznosti)]
      )
      my_colours <- c("red", "darkgrey", "blue")
    
    
    ###########################################################################
    
    ## vykresluji konečný barplot
    
    par(mar = c(3, 6, 3, 3))
    
    barplot(
      my_sample[, (floor(length(obtiznosti) / 2) + 1):length(obtiznosti)],
      beside = TRUE,
      col = my_colours,
      xlab = "",
      ylim = c(0.0, 1.0),
      main = paste(
        "Diagram obtížnosti vs. diskriminace (těžší polovina otázek)",
        "\nvšeobecné lékařství",
                   sep = ""),
      horiz = FALSE
    )
    
    title(ylab = "číslo otázky (řazeno dle obtížnosti)",
          line = 4)
    
            abline(h = c(0.2, 0.4), lty = 2)
            
            axis(side = 1,
                 at = seq(2.5, 2.5 + 4 * (dim(my_data)[2] - 2), by = 4)[
                   1:floor(length(obtiznosti) / 2)
                   ],
                 labels = colnames(my_data)[
                   2:dim(my_data)[2]
                   ][order(obtiznosti)][
                     (floor(length(obtiznosti) / 2) + 1):length(obtiznosti)
                     ],
                 las = 2
            )
            
            legend(x = "topleft",
                   inset = c(0.0, 0.0),
                   legend = c(
                     "obtížnost",
                     "diskriminace (rozdíl podílu úspěšných 5. a 1. pětiny)",
                     "diskriminace (rozdíl podílu úspěšných 5. a 4. pětiny)"
                   ),
                   pch = 19,
                   col = my_colours,
                   bg = "white",
                   title = expression(bold("legenda")),
                   cex = 0.7)
                
  }


## ----------------------------------------------------------------------------

getMySecondHalfDifficultyDiscriminationPlot <- function(my_data){
    
    ###########################################################################
    
    ## doluju bodová skóre pro všechny uchazeče
    
    my_scores <- getMyScores(my_data)
    
    
    ###########################################################################
    
    ## vytvářím nula-jedničkovou tranformaci dle správnosti odpovědí,
    ## nadále už nebudou původní odpovědi uhcazečů třeba
    
    my_data <- getMyTrueFalseTable(my_data)
    
    
    ###########################################################################
    
    ## vytvářím kvintily
    
    my_quintiles <- NULL
    
    for(i in 0:5){
      my_quintiles <- c(my_quintiles,
                        quantile(my_scores, probs = 0.2*i, names = FALSE)
      )
    }
    
    my_quintiles[c(1, 6)] <- c(0, (dim(my_data)[2] - 1) + 1)
    my_quintiles <- ceiling(my_quintiles)
    
    
    ###########################################################################
    
    ## vytvářím bary
    
    ## obtížnost otázky chápu jako 1 - podíl správně odpovídajících studentů
    ## ku všem studentům odpovídajícím na otázku
    
    obtiznosti <- NULL
    
    for(i in 2:dim(my_data)[2]){
      obtiznosti <- c(obtiznosti,
                      1 - length(which(my_data[, i]))/length(my_data[, i])
      )
    }
    
    
    ###########################################################################
    
    ## diskriminační schopnost otázky chápu jako podíl správně odpovídajících
    ## studentů ku všem studentům odpovídajícím na otázku určitého kvantilu,
    ## to celé lomenu podílem správně odpovídajících studentů ku všem
    ## studentům odpovídajícím na otázku určitého jiného kvantilu
    
    ## diskriminace jako 5. kvintil - 4. kvintil
    ## i jako 5. kvintil - 1. kvintil
    
    
    ###########################################################################
    
    diskriminace_nizsi <- NULL
    
    for(i in 2:dim(my_data)[2]){
      diskriminace_nizsi <- c(
        diskriminace_nizsi,
        ((sum(
          my_data[which(my_scores >= my_quintiles[5]), i]
        ) / length(
          my_data[which(my_scores >= my_quintiles[5]), i]
        )) - (sum(
          my_data[which(my_scores < my_quintiles[2]), i]
        ) / length(
          my_data[which(my_scores < my_quintiles[2]), i]
        )))
      )
    }
    
    
    ###########################################################################
    
    diskriminace_vyssi <- NULL
    
    for(i in 2:dim(my_data)[2]){
      diskriminace_vyssi <- c(
        diskriminace_vyssi,
        ((sum(
          my_data[which(my_scores >= my_quintiles[5]), i]
        ) / length(
          my_data[which(my_scores >= my_quintiles[5]), i]
        )) - (sum(
          my_data[which(my_scores < my_quintiles[5] &
                          my_scores >= my_quintiles[4]), i]
        ) / length(
          my_data[which(my_scores < my_quintiles[5] &
                          my_scores >= my_quintiles[4]), i]
        )))
      )
    }
    
    
    ###########################################################################
    
    ## koriguji velmi záporné hodnoty diskriminačních měr, to kvůli
    ## komfortnějšímu vykreslení v diagramu, aby záporné sloupečky
    ## nezasahovaly do popisků položek
    
    diskriminace_nizsi[which(diskriminace_nizsi < 0)] <- -0.01
    diskriminace_vyssi[which(diskriminace_vyssi < 0)] <- -0.01
    
    
    ###########################################################################
    
    ## určuji soubor k vykreslení
    
      my_sample <- rbind("obtiznost" = obtiznosti[order(obtiznosti)],
                         "5p_1p" = diskriminace_nizsi[order(obtiznosti)],
                         "5p_4p" = diskriminace_vyssi[order(obtiznosti)]
      )
      my_colours <- c("red", "darkgrey", "blue")
    
    
    ###########################################################################
    
    ## vykresluji konečný barplot
    
    par(mar = c(3, 6, 3, 3))
    
    barplot(
      my_sample[, 1:floor(length(obtiznosti) / 2)],
      beside = TRUE,
      col = my_colours,
      xlab = "",
      ylim = c(0.0, 1.0),
      main = paste("Diagram obtížnosti vs. diskriminace ",
                   "(lehčí polovina otázek)",
                   "\nvšeobecné lékařství",
                   sep = ""),
      horiz = FALSE
    )
    
    title(ylab = "číslo otázky (řazeno dle obtížnosti)",
          line = 4)
    
            abline(h = c(0.2, 0.4), lty = 2)
            
            axis(side = 1,
                 at = seq(2.5, 2.5 + 4 * (dim(my_data)[2] - 2), by = 4)[
                   1:floor(length(obtiznosti)/2)
                   ],
                 labels = colnames(my_data)[
                   2:dim(my_data)[2]
                   ][order(obtiznosti)][
                     1:floor(length(obtiznosti)/2)
                     ],
                 las = 2
            )
            
            legend(x = "topleft",
                   inset = c(0.0, 0.0),
                   legend = c(
                     "obtížnost",
                     "diskriminace (rozdíl podílu úspěšných 5. a 1. pětiny)",
                     "diskriminace (rozdíl podílu úspěšných 5. a 4. pětiny)"
                   ),
                   pch = 19,
                   col = my_colours,
                   bg = "white",
                   title = expression(bold("legenda")),
                   cex = 0.7)            

  }


## ----------------------------------------------------------------------------

  ## funkce pro vykreslení separátního diagramu na celkovou úspěšnost
  ## jednotlivých pětin uchazečů ----------------------------------------------
  
  getMyOverallSuccessRatePlot <- function(my_data, my_item){
    
    ###########################################################################
    
    ## doluju bodová skóre pro všechny uchazeče
    
    my_scores <- getMyScores(my_data)
    
    
    ###########################################################################
    
    ## vytvářím nula-jedničkovou tranformaci dle správnosti odpovědí
    
    my_data <- getMyTrueFalseTable(my_data)
    
    
    ###########################################################################
    
    ## vytvářím kvintily
    
    my_quintiles <- NULL
    
    for(i in 0:5){
      my_quintiles <- c(my_quintiles,
                        quantile(my_scores, probs = 0.2*i, names = FALSE)
      )
    }
    
    my_quintiles[c(1, 6)] <- c(0, (dim(my_data)[2] - 1) + 1)
    my_quintiles <- ceiling(my_quintiles)
    
    
    ###########################################################################
    
    ## vytvářím bary
    
    my_bars <- NULL
    
    for(i in 1:5){
      my_bars <- c(
        my_bars,
        sum(
          my_data[which(
            my_quintiles[i] <= my_scores &
              my_scores < my_quintiles[i + 1]
          ),
          as.character(my_item)
          ]) / length(
            my_data[which(
              my_quintiles[i] <= my_scores &
                my_scores < my_quintiles[i + 1]
            ),
            as.character(my_item)
            ])
      )
    }
    
    
    ###########################################################################
    
    ## moje popisky barů
    
    my_labels <- NULL
    
    for(i in 1:5){
      my_labels <- c(
        my_labels,
        paste(my_quintiles[i], my_quintiles[i + 1] - 1,
              sep = " - ")
      )
    }
    
    
    ###########################################################################
    
    ## vytvářím barplot s úspěšnosti jednotlivých pětin dané položky
    
    par(mar = c(8, 5, 6, 8), xpd = TRUE)
    
    plot(
      c(0.5:4.5),
      my_bars,
      ylim = c(0.0, 1.0),
      col = "green",
      pch = 19,
      type = "b",
      xlab = "celkový počet bodů",
      xaxt = "n",
      ylab = "relativní četnost odpovědi",
      main = paste(
        "Úspěšnost jednotlivých pětin dle celkového počtu bodů, položka ",
        my_item, "\nvšeobecné lékařství",
        sep = "")
    )
    
    points(
      c(0.5:4.5),
      1 - my_bars,
      type = "b",
      col = "red",
      pch = 19
    )
    
    axis(side = 1,
         at = c(0.5:4.5),
         labels = my_labels
    )
    
    legend(x = "topright",
           inset = c(-0.15, 0),
           legend = c("správná", "špatná"),
           pch = 19,
           col = c("green", "red"),
           title = "odpověď"
    )
    
    
    ###########################################################################
    
    
  }


## ----------------------------------------------------------------------------

## funkce pro vykreslení separátního diagramu na celkovou detailní úspěšnost
## jednotlivých pětin uchazečů ------------------------------------------------
  
  getMyDetailedSuccessRatePlot <- function(
    my_data,
    my_item
  ){

    ###########################################################################
    
    ## vytvářím data.frame s informacemi o odpovědích pro každou možnost A, B,
    ## C, D pro každého uchazeče
    
    odpovedi <- c("A", "B", "C", "D")
    
    if(sum(grepl("id", colnames(my_data))) > 0){
      temp_data <- as.data.frame(
        my_data[, which(grepl("id", colnames(my_data)))[1]]
      )
    }else{
      temp_data <- data.frame(
        "id" = rep(as.factor(rep(NA, dim(my_data)[1])), 4)
      )
    }
    
    for(i in which(grepl(pattern = "[0-9]+", x = colnames(my_data)))){
      
      new_column <- NULL
      
      for(letter in odpovedi){
        new_column <- c(
          new_column,
          grepl(pattern = letter, x = my_data[,i])
        )
      }
      
      temp_data <- as.data.frame(cbind(temp_data, new_column))
    }
    
    dummy_odpovedi <- NULL
    
    for(symbol in odpovedi){
      dummy_odpovedi <- c(dummy_odpovedi, rep(symbol, dim(my_data)[1]))
    }
    
    temp_data <- as.data.frame(cbind(temp_data, dummy_odpovedi))
    
    colnames(temp_data)<-c(
      "id",
      colnames(my_data)[which(grepl(pattern = "[0-9]+", x = colnames(my_data)))],
      "moznost"
    )
    
    assign("odpovedni_arch", value = temp_data)
    
    
    ###########################################################################
    
    ## vytvářím klíče, tj. čtveřici TRUE-FALSE hodnot, kdy TRUE u dané
    ## možnosti značí, že možnost je součástí kombinace správné opdpovědi
    
    temp_data <- as.data.frame(odpovedi)
    
    for(i in which(grepl(pattern = "[0-9]+", x = colnames(my_data)))){
      
      if("X" %in% my_data[, i]){
        
        klic <- rep(TRUE, 4)
        
      }else{
        
        j <- 1
        
        while(!grepl(pattern = "X",x = my_data[j, i]) & j <= dim(my_data)[1]){
          j <- j + 1
        }
        
        klic <- NULL
        
        if(j <= dim(my_data)[1]){
          for(k in 1:length(odpovedi)){
            klic <- c(klic,
                      grepl(pattern = odpovedi[k], x = my_data[j, i]))
          }
        }
        
        
        ## vytvářím klíč, pokud žádný uchazeč neodpoví na položku správně
        
        if(j == (dim(my_data)[1] + 1)){
          klic <- rep(FALSE, 4)
        }
        
      }
      
      temp_data <- as.data.frame(cbind(temp_data, klic))
      
    }
    
    colnames(temp_data) <- c(
      "moznost",
      colnames(my_data)[which(
        grepl(pattern = "[0-9]+", x = colnames(my_data))
      )]
    )
    
    assign("klicovy_arch", value = temp_data)
    
    
    ###########################################################################
    
    ## doluju bodová skóre pro všechny uchazeče
    
    my_scores <- getMyScores(my_data)
    

    ###########################################################################
    
    ## vytvářím nula-jedničkovou tranformaci dle správnosti odpovědí
    
    my_data <- getMyTrueFalseTable(my_data)
    
    
    ###########################################################################
    
    ## vytvářím kvintily
    
    my_quintiles <- NULL
    
    for(i in 0:5){
      my_quintiles <- c(my_quintiles,
                        quantile(my_scores, probs = 0.2*i, names = FALSE)
      )
    }
    
    my_quintiles[c(1, 6)] <- c(0, (dim(my_data)[2] - 1) + 1)
    my_quintiles <- ceiling(my_quintiles)
    
    
    ###########################################################################
    
    ## vytvářím bary
    
    my_bars <- NULL
    
    for(i in 1:5){
      my_bars <- c(
        my_bars,
        sum(
          my_data[which(
            my_quintiles[i] <= my_scores &
              my_scores < my_quintiles[i + 1]
          ),
          as.character(my_item)
          ]) / length(
            my_data[which(
              my_quintiles[i] <= my_scores &
                my_scores < my_quintiles[i + 1]
            ),
            as.character(my_item)
            ])
      )
    }
    
    
    ###########################################################################
    
    ## moje popisky barů
    
    my_labels <- NULL
    
    for(i in 1:5){
      my_labels <- c(
        my_labels,
        paste(my_quintiles[i], my_quintiles[i + 1] - 1,
              sep = " - ")
      )
    }
    
    
    ###########################################################################
    
    ## vytvářím hodnoty pro četnosti jednotlivých odpovědí
    
    for(letter in odpovedi){
      
      temp_data <- NULL
      
      for(i in 1:5){
        
        temp_data <- c(
          temp_data,
          sum(
            subset(
              odpovedni_arch, moznost == letter)[
                which(
                  my_quintiles[i] <= my_scores &
                    my_scores < my_quintiles[i + 1]
                ),
                as.character(my_item)]
          ) / length(
            subset(odpovedni_arch, moznost == letter)[
              which(
                my_quintiles[i] <= my_scores &
                  my_scores < my_quintiles[i + 1]),
              as.character(my_item)]
          )
        )
        
      }
      
      assign(paste(letter, "cetnost", sep = "_"), temp_data)
      
    }
    
    
    ###########################################################################
    
    ## vytvářím klíč pro uživatelem vybranou položku
    
    my_key <- klicovy_arch[, as.character(my_item)]
    
    
    ###########################################################################
    
    ## vytvářím typy čar do legendy
    
    my_lty <- NULL
    
    for(letter in odpovedi){
      my_lty <- c(my_lty,
                  if(which(letter == odpovedi) %in% which(my_key)){1}else{2})
    }
    
    
    ###########################################################################
    
    ## vytvářím barplot s charakteristikami položky
    
    par(mar = c(8, 5, 6, 8), xpd = TRUE)
    
    barplot(
      my_bars,
      space = rep(0, 5),
      ylim = c(0, 1),
      col = "lightgrey",
      xlab = "celkový počet bodů",
      names = my_labels,
      ylab = "relativní četnost odpovědi"
    )
    
    title(
      main = paste("Psychometrické charakteristiky, položka ",
                   my_item,
                   "\nvšeobecné lékařství",
                   sep = ""),
      line = 3
    )
    
    for(letter in odpovedi){
      points(
        c(0.5:4.5),
        get(paste(letter, "cetnost", sep = "_")),
        type = "b",
        col = which(letter == odpovedi),
        pch = which(letter == odpovedi),
        lty = if(my_key[which(letter == odpovedi)]){1}else{2}
      )
    }
    
    legend(x = "topright",
           inset = c(-0.15, 0),
           legend = c(
             if(1 %in% which(my_key)){expression(bold("A"))}else{"A"},
             if(2 %in% which(my_key)){expression(bold("B"))}else{"B"},
             if(3 %in% which(my_key)){expression(bold("C"))}else{"C"},
             if(4 %in% which(my_key)){expression(bold("D"))}else{"D"}
           ),
           pch = c(1:4),
           col = c(1:4),
           lty = my_lty,
           title = "odpověď")
    
    
    ###########################################################################
    
    
  }


## ----------------------------------------------------------------------------

## funkce pro vykreslení separátního diagramu na celkovou úspěšnost
## jednotlivých pětin uchazečů ------------------------------------------------
  
  getMyAnswerSchemaPlot <- function(
    my_data,
    my_item
  ){
      
    ###########################################################################
    
    ## vytvářím celé spektrum možných odpovědí
    
    vsechny_moznosti <- c(
      "",
      "A", "B", "C", "D",
      "AB", "AC", "AD", "BC", "BD", "CD",
      "ABC", "ABD", "ACD", "BCD",
      "ABCD"
    )
    
    
    ###########################################################################
    
    ## vytvářím tabulku s četnostmi jednotlivých kombinací odpovědí
    
    my_table <- matrix(rep(0, 16), nrow = 1)
    
    my_table <- as.data.frame(my_table)
    
    colnames(my_table) <- vsechny_moznosti
    
    
    ## tabulka s četnostmi jednotlivých kombinací odpovědí
    
    item_table <- table(my_data[, as.character(my_item)])
    
    my_names <- names(item_table)
    
    for(name in names(item_table)){
      if(grepl("X", name)){
        my_names[which(name == names(item_table))] <- gsub("X", "", name)
      }
    }
    
    for(i in 1:length(item_table)){
      for(j in 2:length(vsechny_moznosti)){
        if(my_names[i] == vsechny_moznosti[j]){
          my_table[1, vsechny_moznosti[j]] <- item_table[i]
        }
      }
    }
    
    for(i in 1:length(names(item_table))){
      if(names(item_table)[i] == "" | names(item_table)[i] == "X"){
        my_table[1, 1] <- item_table[i]
      }
    }
    
    colnames(my_table)[1] <- "bez odpovědi"
    
    
    ###########################################################################
    
    ## vytvářím vlastní popisky osy x
    
    if(any(names(item_table) == "X")){
      
      my_index <- c(1:16)
      my_long_labels <- rep("", 16)
      
    }else{
      
      my_index <- 1
      
      if(sum(grepl("X", names(item_table)))){
        for(i in 1:length(vsechny_moznosti)){
          if(vsechny_moznosti[i] == gsub(
            "X",
            "",
            names(item_table)[which(grepl("X", names(item_table)))])){
            my_index <- i
          }
        }
      }
      
      my_long_labels <- c(
        expression(symbol("\306")),
        colnames(my_table)[2:16]
      )
      
      my_long_labels[my_index] <- ""
      
    }
    
    
    ###########################################################################
    
    ## vytvářím data.frame s informacemi o odpovědích pro každou možnost A, B,
    ## C, D pro každého uchazeče
    
    odpovedi <- c("A", "B", "C", "D")
    
    if(sum(grepl("id", colnames(my_data))) > 0){
      temp_data <- as.data.frame(
        my_data[, which(grepl("id", colnames(my_data)))[1]]
      )
    }else{
      temp_data <- data.frame(
        "id" = rep(as.factor(rep(NA, dim(my_data)[1])), 4)
      )
    }
    
    for(i in which(grepl(pattern = "[0-9]+", x = colnames(my_data)))){
      
      new_column <- NULL
      
      for(letter in odpovedi){
        new_column <- c(
          new_column,
          grepl(pattern = letter, x = my_data[,i])
        )
      }
      
      temp_data <- as.data.frame(cbind(temp_data, new_column))
    }
    
    dummy_odpovedi <- NULL
    
    for(symbol in odpovedi){
      dummy_odpovedi <- c(dummy_odpovedi, rep(symbol, dim(my_data)[1]))
    }
    
    temp_data <- as.data.frame(cbind(temp_data, dummy_odpovedi))
    
    colnames(temp_data)<-c(
      "id",
      colnames(my_data)[which(grepl(pattern = "[0-9]+", x = colnames(my_data)))],
      "moznost"
    )
    
    assign("odpovedni_arch", value = temp_data)
    
    
    ###########################################################################
    
    ## vytvářím klíče, tj. čtveřici TRUE-FALSE hodnot, kdy TRUE u dané
    ## možnosti značí, že možnost je součástí kombinace správné opdpovědi
    
    temp_data <- as.data.frame(odpovedi)
    
    for(i in which(grepl(pattern = "[0-9]+", x = colnames(my_data)))){
      
      if("X" %in% my_data[, i]){
        
        klic <- rep(TRUE, 4)
        
      }else{
        
        j <- 1
        
        while(!grepl(pattern = "X",x = my_data[j, i]) & j <= dim(my_data)[1]){
          j <- j + 1
        }
        
        klic <- NULL
        
        if(j <= dim(my_data)[1]){
          for(k in 1:length(odpovedi)){
            klic <- c(klic,
                      grepl(pattern = odpovedi[k], x = my_data[j, i]))
          }
        }
        
        
        ## vytvářím klíč, pokud žádný uchazeč neodpoví na položku správně
        
        if(j == (dim(my_data)[1] + 1)){
          klic <- rep(FALSE, 4)
        }
        
      }
      
      temp_data <- as.data.frame(cbind(temp_data, klic))
      
    }
    
    colnames(temp_data) <- c(
      "moznost",
      colnames(my_data)[which(
        grepl(pattern = "[0-9]+", x = colnames(my_data))
      )]
    )
    
    assign("klicovy_arch", value = temp_data)
    
    
    ###########################################################################
    
    ## doluju bodová skóre pro všechny uchazeče
    
    my_scores <- getMyScores(my_data)
    
    
    ###########################################################################
    
    ## vytvářím nula-jedničkovou tranformaci dle správnosti odpovědí
    
    my_data <- getMyTrueFalseTable(my_data)
    
    
    ###########################################################################
    
    ## vytvářím klíč pro uživatelem vybranou položku
    
    my_key <- klicovy_arch[, as.character(my_item)]
    
    
    ###########################################################################
    
    ## vytvářím diagram s relativními četnostmi jednotlivých kombinací
    ## odpovědí
    
    par(mar = c(8, 5, 6, 4), xpd = TRUE)
    
    barplot(
      t(my_table)[, 1] / sum(my_table[1, ]),
      ylim = c(0.0, 1.0),
      xlab = "vzorec odpovědi",
      ylab = "relativní četnost odpovědi",
      xaxt = "n",
      main = paste(
        "Relativní četnost jednotlivých kombinací odpovědí, položka ",
        my_item,
        "\nvšeobecné lékařství",
        sep = "")
    )
    
    text(seq(0.65, 0.65 + 15 * 1.205, 1.205),
         y = -0.06,
         labels = my_long_labels,
         srt = 45)
    
    if(length(my_index) == 1){
      
      text(x = 0.65 + 1.205 * (my_index - 1),
           y = -0.06,
           labels = if(my_index == 1){
             expression(symbol("\306"))
           }else{
             gsub("X",
                  "",
                  names(item_table)[which(grepl("X", names(item_table)))])},
           font = 2,
           srt = 45)
      
    }
    
    if(length(my_index) > 1){
      
      text(x = 0.65,
           y = -0.06,
           labels = expression(symbol("\306")),
           font = 2,
           srt = 45)
      
      text(x = seq(0.65 + 1.205, 0.65 + 1.205 * (length(my_index) - 1), 1.205),
           y = -0.06,
           labels = names(my_table)[2:16],
           font = 2,
           srt = 45)
      
    }
    
    
    ###########################################################################
    
    
  }


## ----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################

## loaduju data ---------------------------------------------------------------

setwd(mother_working_directory)

my_data <- read.csv(
    paste(
       "https://raw.githubusercontent.com/LStepanek",
       "Nekolik-postrehu-k-teorii-odpovedi-na-polozku/master/my_data.csv",
       sep = "/"
       ),
       sep = ";",
       skip = 1,
       check.names = FALSE,
       encoding = "UTF-8"
)


## ----------------------------------------------------------------------------

###############################################################################

## preprocessing --------------------------------------------------------------

data <- my_data

if(!is.null(my_data)){

    ## hledám, zda je přítomna proměnná s rodným číslem;
    ## pokud ano, přejmenovávám ji na 'id'
    ## a raději ji kóduju jako factor

    for(i in 1:length(colnames(my_data))){
    
        if(
            grepl("rodné.číslo", tolower(colnames(my_data)[i])) |
            grepl("rodcislo", tolower(colnames(my_data)[i]))
        ){
            colnames(my_data)[i] <- "id"
            my_data[,i] <- as.factor(as.character(my_data[, i]))
        }
    
    }


    ## pokud dataset obsahuje proměnnou 'obor' či 'kobor',
    ## z datasetů extrahuji jen data podmnožiny ucházející
    ## se o studium lékařství ('LEK')

    for(i in 1:length(colnames(my_data))){
        if(
            grepl("obor", tolower(colnames(my_data)[i]))
        ){
            if("51418" %in% levels(my_data[, i])){
                my_data <- subset(my_data, my_data[, i] == "51418")
            }else{
                my_data <- subset(my_data, my_data[, i] == "LEK")
            }
        }
    }


    ## pokud dataset obsahuje proměnnou 'kolo', z datasetů
    ## extrahuji jen data pro první kolo přijímacích zkoušek

    for(i in 1:length(colnames(my_data))){
        if(
            grepl("kolo", x = tolower(colnames(my_data)[i]))
        ){
            my_data <- subset(my_data, my_data[, i] == "1")
        }
    }
    
    
    ## nakonec ještě přetypovávám kategorické proměnné, aby
    ## měly správný počet levelů
    
    for(i in 1:dim(my_data)[2]){
        my_data[,i] <- as.character(my_data[, i])
    }

    for(i in 1:dim(my_data)[2]){
        my_data[,i] <- as.factor(my_data[, i])
    }

    for(i in which(grepl("[0-9]+", colnames(my_data)))){
        my_data[, i] <- as.character(my_data[, i])
    }


}


## ----------------------------------------------------------------------------

###############################################################################

## vytvářím pro každou položku každého modulu a ročníku všechny typy
## diagramů -------------------------------------------------------------------

year <- "2020"
predmet <- "biologie"

setwd(
    paste(
        mother_working_directory,
        "diagramy_nad_vysledky_uchazecu",
        sep = "/"
        )
    )


## ----------------------------------------------------------------------------

if(!is.null(paste(predmet,year,sep="_"))){

data <- my_data


## ----------------------------------------------------------------------------

png(
    filename = paste("histogram_20_",predmet,"_",year,".png",sep=""),
    width=8,
    height=5,
    units="in",
    res=600
)

getMyHistogram(data,20)

dev.off()


## ----------------------------------------------------------------------------

png(
    filename = paste("histogram_100_",predmet,"_",year,".png",sep=""),
    width=8,
    height=5,
    units="in",
    res=600
)

getMyHistogram(data,100)

dev.off()


## ----------------------------------------------------------------------------

png(
    filename = paste("holy_trinity_",predmet,"_",year,".png",sep=""),
    width=24,
    height=8,
    units="in",
    res=600
)

getMyDifficultyDiscriminationPlot(data)

dev.off()


## ----------------------------------------------------------------------------

png(
    filename = paste("holy_trinity_harder_",predmet,"_",year,".png",sep=""),
    width=12,
    height=8,
    units="in",
    res=600
)

getMyFirstHalfDifficultyDiscriminationPlot(data)

dev.off()


## ----------------------------------------------------------------------------

png(
    filename = paste("holy_trinity_easier_",predmet,"_",year,".png",sep=""),
    width=12,
    height=8,
    units="in",
    res=600
)

getMySecondHalfDifficultyDiscriminationPlot(data)

dev.off()


## ----------------------------------------------------------------------------

for(my_item in colnames(data)[which(grepl("[0-9]+",colnames(data)))]){

flush.console()
print(
    paste(
        "ročník ",year,", předmět ",predmet,", ",
        format(
        which(
        colnames(data)[grepl("[0-9]+",colnames(data))]==my_item
        )/length(
        which(grepl("[0-9]+",colnames(data)))
        )*100,nsmall=2
        ),
        " %",
        sep=""
    )
)

png(
    filename = paste(
    "overall_success_rate_item_",my_item,"_",predmet,"_",year,".png",sep=""
    ),
    width=10,
    height=6.5,
    units="in",
    res=600
)

getMyOverallSuccessRatePlot(data,my_item)

dev.off()


png(
    filename = paste(
    "detailed_success_rate_item_",my_item,"_",predmet,"_",year,".png",sep=""
    ),
    width=8,
    height=6.5,
    units="in",
    res=600
)

getMyDetailedSuccessRatePlot(data,my_item)

dev.off()


png(
    filename = paste(
    "answer_schema_plot_item_",my_item,"_",predmet,"_",year,".png",sep=""
    ),
    width=8,
    height=6.5,
    units="in",
    res=600
)

getMyAnswerSchemaPlot(data,my_item)

dev.off()


}


## ----------------------------------------------------------------------------

}


## ----------------------------------------------------------------------------

###############################################################################




## ----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################






