{
    #the goal here is to load all the broken streets and then get the p-vals
    #This is for ARRESTS
    TwoBuf <- function(line,width,minEx){
      Buf0 <- gBuffer(line,width=minEx,capStyle="SQUARE")
      Buf1 <- gBuffer(line,width=width,capStyle="FLAT")
      return(disaggregate(gDifference(Buf1,Buf0)))
    }

    arimaTestNullBroken2new <- function(bufferWidth, i, longStrBroke) {

      buffIndex <- bufferWidth / 10
      index2 = bufferWidth / 100
      endResults <- vector(mode = "list", length = length(longStrBroke[[i]]))
      #Filter my arrest and offense data
      precIndex = nyc$Precinct[i]
      data_arrest <- dplyr::filter(dataArrF, arrest_precinct == precIndex)
      data_offense <- dplyr::filter(dataOffFiltered, precinct == precIndex)

      #Adding monthkey
      data_arrest <- data_arrest %>%
        dplyr::mutate(monthKey = as.numeric(paste0(data_arrest$month, data_arrest$year)))
      data_offense <- data_offense %>%
        dplyr::mutate(monthKey = as.numeric(paste0(data_offense$month, data_offense$year)))

        print(paste0("i: ", i))
        for(j in 1:length(longStrBroke[[i]])) {
          print(paste0("j: ", j))
          endResults[[j]] = vector(mode = "list", length = length(longStrBroke[[i]][[j]]))
          for(k in 1:length(longStrBroke[[i]][[j]])) {
            endResults[[j]][[k]] = NULL #initialize to null
            if(!is.null(longStrBroke[[i]][[j]][[k]])) {

              myStreet <- longStrBroke[[i]][[j]][[k]]$shorterStreet

              #Create the polygon buffer
              myPolyBIG <- TwoBuf(line=myStreet,width=bufferWidth,minEx=0.0001)
              myPolySMALL <- TwoBuf(line=myStreet,width=50,minEx=0.0001)


              if(length(myPolyBIG@polygons) >= 2 & length(myPolySMALL@polygons) >= 2) {

              a_BIG_1 <- point.in.polygon(data_arrest$x_coord_cd, data_arrest$y_coord_cd,
                                          myPolyBIG@polygons[[1]]@Polygons[[1]]@coords[,1],
                                          myPolyBIG@polygons[[1]]@Polygons[[1]]@coords[,2])
              a_SMALL_1 <- point.in.polygon(data_arrest$x_coord_cd, data_arrest$y_coord_cd,
                                            myPolySMALL@polygons[[1]]@Polygons[[1]]@coords[,1],
                                            myPolySMALL@polygons[[1]]@Polygons[[1]]@coords[,2])
              a_BIG_2 <- point.in.polygon(data_arrest$x_coord_cd, data_arrest$y_coord_cd,
                                          myPolyBIG@polygons[[2]]@Polygons[[1]]@coords[,1],
                                          myPolyBIG@polygons[[2]]@Polygons[[1]]@coords[,2])
              a_SMALL_2 <- point.in.polygon(data_arrest$x_coord_cd, data_arrest$y_coord_cd,
                                            myPolySMALL@polygons[[2]]@Polygons[[1]]@coords[,1],
                                            myPolySMALL@polygons[[2]]@Polygons[[1]]@coords[,2])

              o_BIG_1 <- point.in.polygon(data_offense$x_coord_cd, data_offense$y_coord_cd,
                                          myPolyBIG@polygons[[1]]@Polygons[[1]]@coords[,1],
                                          myPolyBIG@polygons[[1]]@Polygons[[1]]@coords[,2])
              o_SMALL_1 <- point.in.polygon(data_offense$x_coord_cd, data_offense$y_coord_cd,
                                            myPolySMALL@polygons[[1]]@Polygons[[1]]@coords[,1],
                                            myPolySMALL@polygons[[1]]@Polygons[[1]]@coords[,2])
              o_BIG_2 <- point.in.polygon(data_offense$x_coord_cd, data_offense$y_coord_cd,
                                          myPolyBIG@polygons[[2]]@Polygons[[1]]@coords[,1],
                                          myPolyBIG@polygons[[2]]@Polygons[[1]]@coords[,2])
              o_SMALL_2 <- point.in.polygon(data_offense$x_coord_cd, data_offense$y_coord_cd,
                                            myPolySMALL@polygons[[2]]@Polygons[[1]]@coords[,1],
                                            myPolySMALL@polygons[[2]]@Polygons[[1]]@coords[,2])
              #Getting the actual data points
              a_points_B_1 <- data_arrest[c(which(a_BIG_1 > 0)), ]
              a_points_B_2 <- data_arrest[c(which(a_BIG_2 > 0)), ]
              a_points_S_1 <- data_arrest[c(which(a_SMALL_1 > 0)), ]
              a_points_S_2 <- data_arrest[c(which(a_SMALL_2 > 0)), ]

              o_points_B_1 <- data_offense[c(which(o_BIG_1 > 0)), ]
              o_points_B_2 <- data_offense[c(which(o_BIG_2 > 0)), ]
              o_points_S_1 <- data_offense[c(which(o_SMALL_1 > 0)), ]
              o_points_S_2 <- data_offense[c(which(o_SMALL_2 > 0)), ]

              #Getting into MONTH_YEAR format
              df_a_B_1 <- data.frame(table(a_points_B_1$monthKey))
              df_a_B_2 <- data.frame(table(a_points_B_2$monthKey))
              df_a_S_1 <- data.frame(table(a_points_S_1$monthKey))
              df_a_S_2 <- data.frame(table(a_points_S_2$monthKey))

              df_o_B_1 <- data.frame(table(o_points_B_1$monthKey))
              df_o_B_2 <- data.frame(table(o_points_B_2$monthKey))
              df_o_S_1 <- data.frame(table(o_points_S_1$monthKey))
              df_o_S_2 <- data.frame(table(o_points_S_2$monthKey))

              freqB1 = freqB2 = freqS1 = freqS2 = freqOffB1 = freqOffB2 = freqOffS1 = freqOffS2 = monthYearKey

              if(nrow(a_points_B_1) != 0) {
                freqB1$Freq <- df_a_B_1$Freq[match(freqB1$Var1, df_a_B_1$Var1)]
                freqB1$Freq[is.na(freqB1$Freq)] <- 0
              }
              if(nrow(a_points_B_2) != 0) {
                freqB2$Freq <- df_a_B_2$Freq[match(freqB2$Var1, df_a_B_2$Var1)]
                freqB2$Freq[is.na(freqB2$Freq)] <- 0
              }
              if(nrow(a_points_S_1) != 0) {
                freqS1$Freq <- df_a_S_1$Freq[match(freqS1$Var1, df_a_S_1$Var1)]
                freqS1$Freq[is.na(freqS1$Freq)] <- 0
              }
              if(nrow(a_points_S_2) != 0) {
                freqS2$Freq <- df_a_S_2$Freq[match(freqS2$Var1, df_a_S_2$Var1)]
                freqS2$Freq[is.na(freqS2$Freq)] <- 0
              }

              if(nrow(o_points_B_1) != 0) {
                freqOffB1$Freq <- df_o_B_1$Freq[match(freqOffB1$Var1, df_o_B_1$Var1)]
                freqOffB1$Freq[is.na(freqOffB1$Freq)] <- 0
              }
              if(nrow(o_points_B_2) != 0) {
                freqOffB2$Freq <- df_o_B_2$Freq[match(freqOffB2$Var1, df_o_B_2$Var1)]
                freqOffB2$Freq[is.na(freqOffB2$Freq)] <- 0
              }
              if(nrow(o_points_S_1) != 0) {
                freqOffS1$Freq <- df_o_S_1$Freq[match(freqOffS1$Var1, df_o_S_1$Var1)]
                freqOffS1$Freq[is.na(freqOffS1$Freq)] <- 0
              }
              if(nrow(o_points_S_2) != 0) {
                freqOffS2$Freq <- df_o_S_2$Freq[match(freqOffS2$Var1, df_o_S_2$Var1)]
                freqOffS2$Freq[is.na(freqOffS2$Freq)] <- 0
              }

              #Final results
              return_pval = 0

              arr1 <- freqB1$Freq - freqS1$Freq
              arr2 <- freqB2$Freq - freqS2$Freq
              off1 <- freqOffB1$Freq - freqOffS1$Freq
              off2 <- freqOffB2$Freq - freqOffS2$Freq

              if(sum(off1 == 0) > 0 || sum(off2 == 0) > 0) {
                off1 <- off1 + 1
                off2 <- off2 + 1
              }

              arr1 <- arr1 / off1
              arr2 <- arr2 / off2

              arrDiff <- data.frame(arr1 - arr2)
              colnames(arrDiff) <- c("difference")

              #Scales the differences to be compatible with "arima"
              newDifference <- arrDiff$difference/sd(arrDiff$difference)
              if(is.na(sum(newDifference))) {
                return_pval <- NA
              }

              else {
                ## Now do a naive test of the means similar to how we were doing it for the policing example
                ## PERFORMING THE ARIMA TEST!! ##
                print("check1")
                myModel = arima(newDifference, order = c(1, 0, 0))
                print("check2")
                SE = sqrt(myModel$var.coef[2,2])
                EST = myModel$coef[2]

                if (EST < 0) {
                  PVALUE = as.numeric(2*pnorm(EST, mean=0, sd=SE))
                } else {
                  PVALUE = as.numeric(2*(1 - pnorm(EST, mean=0, sd=SE)))
                }

                return_pval = PVALUE
              }

              endResults[[j]][[k]] <- list("pValue" = return_pval,
                                            "ID" = longStrBroke[[i]][[j]][[k]]$uniqueID)

              }
            }
          }
        }

      return(endResults)
    }

    index <- as.numeric(Sys.getenv('SLURM_ARRAY_TASK_ID'))
    b = index*10
    c = index * 100
    load(paste0("Output/streets", index, ".dat")) # this gets loaded as "longStrBroke"
    pValInfoArrNew = vector(mode = "list", length = length(longStrBroke))

    for (i in 1:length(pValInfoArrNew)) {
      pValInfoArrNew[[i]] <- arimaTestNullBroken2new(c, i, longStrBroke)
    }
    save(pValInfoArrNew, file=paste("/blue/jantonelli/emmett.kendall/JoeyProject/CrimeProject/OutputNew/pInfoArrNew",
                                 index, ".dat", sep=''))

}

library("sp")
library("sf")
library("rgeos")
library("raster")

trialNum = 1
k = 8; j =150
set.seed(trialNum)

load("../Data/indexList_MAIN.RData")

load(paste0('../Output/nullGridInfo/combinedMatchingSetup', k, ".dat"))
load(paste0('../Output/origGridInfo/sim_orig_', k, '.dat'))

wMax_a = max(na.omit(sim_orig$DATA$area1 / sim_orig$DATA$area2))
wMin_a = min(na.omit(sim_orig$DATA$area1 / sim_orig$DATA$area2))

wMax_s = max(na.omit(sim_orig$DATA$streets1 / sim_orig$DATA$streets2))
wMin_s = min(na.omit(sim_orig$DATA$streets1 / sim_orig$DATA$streets2))

wMatchOk1 = which((combinedMatchingSetupFix$DATA$area1 / combinedMatchingSetupFix$DATA$area2) > wMin_a &
                    (combinedMatchingSetupFix$DATA$area1 / combinedMatchingSetupFix$DATA$area2) < wMax_a &
                    (combinedMatchingSetupFix$DATA$streets1 / combinedMatchingSetupFix$DATA$streets2) > wMin_s &
                    (combinedMatchingSetupFix$DATA$streets1 / combinedMatchingSetupFix$DATA$streets2) < wMax_s)

wMatchOk2 = which(!is.na(combinedMatchingSetupFix$DATA$t_stat_pval))
wMatchOk = intersect(wMatchOk1, wMatchOk2)

combinedMatchingSetupFix2 = combinedMatchingSetupFix
combinedMatchingSetupFix2$DATA = combinedMatchingSetupFix2$DATA[wMatchOk,]
combinedMatchingSetupFix2$ARR_IND_1 = combinedMatchingSetupFix2$ARR_IND_1[wMatchOk]
combinedMatchingSetupFix2$ARR_IND_2 = combinedMatchingSetupFix2$ARR_IND_2[wMatchOk]
combinedMatchingSetupFix2$OFF_IND_1 = combinedMatchingSetupFix2$OFF_IND_1[wMatchOk]
combinedMatchingSetupFix2$OFF_IND_2 = combinedMatchingSetupFix2$OFF_IND_2[wMatchOk]

tot_lengths = data.frame("arr1" = sapply(combinedMatchingSetupFix2$ARR_IND_1, length),
                         "arr2" = sapply(combinedMatchingSetupFix2$ARR_IND_2, length),
                         "off1" = sapply(combinedMatchingSetupFix2$OFF_IND_1, length),
                         "off2" = sapply(combinedMatchingSetupFix2$OFF_IND_2, length))
tot_lengths[which(tot_lengths$off1 == 0 | tot_lengths$off2 == 0), ] = NA

v1 = sd(tot_lengths$off1 + tot_lengths$off2, na.rm=TRUE)^2
rat_off = tot_lengths$off1 / tot_lengths$off2
rat_off[which(rat_off < 1)] = 1 / rat_off[which(rat_off < 1)]
v2 = sd(rat_off, na.rm=TRUE)^2

# Need to compensate for 0s
off_num = data.frame("off1" = sapply(sim_orig$OFF_IND_1, length),
                     "off2" = sapply(sim_orig$OFF_IND_2, length))
off_num[which(off_num$off1 == 0 | off_num$off2 == 0), ] = 
  off_num[which(off_num$off1 == 0 | off_num$off2 == 0), ] + 1

row_num = 1
perc_pval_match[[k]] = data.frame("num_match" = match_count,
                                  "perc_pval_less_05" = rep(NA, length(match_count)))
p_val_df[[k]] = matrix(nrow = length(match_count), ncol = nrow(sim_orig$DATA))

print(paste0("Match Num: ", j))
pval = rep(NA, nrow(sim_orig$DATA))
w50_mat = matrix(nrow = 164, ncol = 150)
for (ii in indexList_MAIN) {
  ## find matches
  off_temp = off_num$off1[ii] + off_num$off2[ii]
  ratio_temp = max(off_num$off1[ii] / off_num$off2[ii],
                   off_num$off2[ii] / off_num$off1[ii])
  
  dist_temp = sqrt(((off_temp - (tot_lengths$off1 + tot_lengths$off2))^2/v1) +
                     ((ratio_temp - rat_off)^2 / v2))
  
  # w50 = order(dist_temp)[1:j]
  
  # Choose one mother street --------------------
  match_counter = jj = 1
  streetInd = vector(mode = "list", length = 77)
  for (w in 1:77) {streetInd[[w]] = c(-1) }
  w50 = rep(NA, j)
  close_ind = order(dist_temp)
  while(match_counter <= j) {
    temp = combinedMatchingSetupFix2$DATA[close_ind[jj], ]
    if(!(temp$indigo %in% streetInd[[temp$precinct]])) {
      w50[match_counter] = close_ind[jj]
      match_counter = match_counter + 1
      streetInd[[temp$precinct]] = append(streetInd[[temp$precinct]], temp$indigo)
    }
    jj = jj + 1
  }
  # --------------------------------------------
  w50_mat[ii, ] = w50
}


# investigating how many unique matches we are actually getting and what the distribution is
w50_total = c(w50_mat); w50_total = na.omit(w50_total)
w50_total = data.frame(sort(table(w50_total), decreasing = T))

common_sub = combinedMatchingSetupFix2$DATA[c(w50_total$w50_total[1:155]), ]
pval_sub = rep(NA, 164)
pval_scale = rep(NA, 164)
for(ii in indexList_MAIN) {
  t_stat_temp = sim_orig$DATA$t_stat_pval[[ii]]
  null_temp = combinedMatchingSetupFix2$DATA$t_stat_pval[w50_mat[ii, ]]
  print(mean(t_stat_temp > null_temp))
  pval_sub[ii] = mean(null_temp > t_stat_temp)
  
  test = density(null_temp, bw = "ucv")
  xx = test$x
  yy = test$y
  dx <- xx[2L] - xx[1L]
  C <- sum(yy) * dx
  
  p.unscaled <- sum(yy[xx >= t_stat_temp]) * dx
  p.scaled <- p.unscaled / C
  
  pval_scale[ii] = p.scaled
  
}
hist(pval_sub)
hist(pval_scale)
cbind(pval_sub, pval_scale)
combinedMatchingSetupFix2$DATA[w50_mat[126, ], ]

hist(common_sub$t_stat_pval, main = "p-vals of 155 most common null streets",
     xlab = "pval")
hist(na.omit(sim_orig$DATA$t_stat_pval), main = "p-vals of 144 observed boundaries",
     xlab = "pval")
