library(sp); library(sf); library(rgeos); library(raster)

load('../Data/dataArr_sub.rda') # dataArr_sub
load('../Data/dataOff_sub.rda') # dataOff_sub
load('../Data/nycSub.RData')
load('../Data/monthKey.rda')

# monthKey = as.vector(unique(dataArr_sub$yearmonth))
# for(i in 1:length(monthKey)) {
#   if(nchar(monthKey[i]) > 6) {
#     temp = paste0(substr(monthKey[i],1,4), "z", substr(monthKey[i],6,7))
#     monthKey[i] = temp
#   }
# }
# monthKey = sort(monthKey)
# for(i in 1:length(monthKey)) {
#   if(nchar(monthKey[i]) > 6) {
#     temp = paste0(substr(monthKey[i],1,4), "m", substr(monthKey[i],6,7))
#     monthKey[i] = temp
#   }
# }
# save(monthKey, file = "../Data/monthKey.rda")
# dataArr_sub$main_ind = 1:nrow(dataArr_sub); save(dataArr_sub, file = '../Data/dataArr_sub.rda')
# dataOff_sub$main_ind = 1:nrow(dataOff_sub); save(dataOff_sub, file = '../Data/dataOff_sub.rda')

# Iterate through each buffer width
for (index in 2:13) {
  
    k = as.numeric(Sys.getenv('SLURM_ARRAY_TASK_ID'))
    set.seed(k)

    prec_num = nycSub$Precinct[k]
    arr_sub = dataArr_sub[dataArr_sub$precinct == prec_num, ]
    off_sub = dataOff_sub[dataOff_sub$precinct == prec_num, ]

    print(paste0("Buffer: ", index, ", Precinct: ", k))

    l = 1000 # arbitrary start length

    nullStr_point_data <- list(DATA = data.frame("precinct" = rep(-1,l), "indigo" = rep(-1,l), "juliet" = rep(-1,l),
                                      "streets1" = rep(-1,l), "streets2" = rep(-1,l),
                                      "area1" = rep(-1,l), "area2" = rep(-1,l), "splitProper" = rep(F,l),
                                      "t_stat_pval" = rep(-1,l), "naive_pval" = rep(-1,l)),
                              ARR_IND_1 = vector(mode = 'list', length = l),
                              ARR_IND_2 = vector(mode = 'list', length = l),
                              OFF_IND_1 = vector(mode = 'list', length = l),
                              OFF_IND_2 = vector(mode = 'list', length = l))
    rowNum = 1

    load(paste0("../Data/OutputStrInfo_noWater/strInfo_", index, "_", k, ".dat")) # contains the buffer object

    print(paste0("Total length: ", length(streetLengthInfo_null)))
    for(i in 1:length(streetLengthInfo_null)) {
      print(paste0("i ", i))
      for(j in 1:length(streetLengthInfo_null[[i]])) {
        if(!is.na(streetLengthInfo_null[[i]][[j]])) {
          if(length(streetLengthInfo_null[[i]][[j]]$buffer@polygons) > 1){
            poly1 = streetLengthInfo_null[[i]][[j]]$buffer@polygons[[1]]
            poly2 = streetLengthInfo_null[[i]][[j]]$buffer@polygons[[2]]

            area1 = poly1@area
            area2 = poly2@area
            
            arr_1 = point.in.polygon(arr_sub$x_coord_cd, arr_sub$y_coord_cd,
                                  poly1@Polygons[[1]]@coords[,1], poly1@Polygons[[1]]@coords[,2])
            arr_2 = point.in.polygon(arr_sub$x_coord_cd, arr_sub$y_coord_cd,
                                  poly2@Polygons[[1]]@coords[,1], poly2@Polygons[[1]]@coords[,2])
            
            off_1 = point.in.polygon(off_sub$x_coord_cd, off_sub$y_coord_cd,
                                     poly1@Polygons[[1]]@coords[,1], poly1@Polygons[[1]]@coords[,2])
            off_2 = point.in.polygon(off_sub$x_coord_cd, off_sub$y_coord_cd,
                                     poly2@Polygons[[1]]@coords[,1], poly2@Polygons[[1]]@coords[,2])
            
            arr_1_ind = arr_sub$main_ind[which(arr_1 > 0)]
            arr_2_ind = arr_sub$main_ind[which(arr_2 > 0)]
            
            off_1_ind = off_sub$main_ind[which(off_1 > 0)]
            off_2_ind = off_sub$main_ind[which(off_2 > 0)]
            
            arr_1_pts = dataArr_sub[arr_1_ind, ]
            arr_2_pts = dataArr_sub[arr_2_ind, ]
            
            off_1_pts = dataOff_sub[off_1_ind, ]
            off_2_pts = dataOff_sub[off_2_ind, ]
            
            df_a_1 <- data.frame(table(arr_1_pts$yearmonth))
            df_a_2 <- data.frame(table(arr_2_pts$yearmonth))
            df_o_1 <- data.frame(table(off_1_pts$yearmonth))
            df_o_2 <- data.frame(table(off_2_pts$yearmonth))
            
            freq_a1 = freq_a2 = freq_o1 = freq_o2 = data.frame("Var1" = monthKey, 
                                                               "Freq" = 0)
            if(nrow(arr_1_pts) != 0) {
              freq_a1$Freq <- df_a_1$Freq[match(freq_a1$Var1, df_a_1$Var1)]
              freq_a1$Freq[is.na(freq_a1$Freq)] <- 0
            }
            if(nrow(arr_2_pts) != 0) {
              freq_a2$Freq <- df_a_2$Freq[match(freq_a2$Var1, df_a_2$Var1)]
              freq_a2$Freq[is.na(freq_a2$Freq)] <- 0
            }
            if(nrow(off_1_pts) != 0) {
              freq_o1$Freq <- df_o_1$Freq[match(freq_o1$Var1, df_o_1$Var1)]
              freq_o1$Freq[is.na(freq_o1$Freq)] <- 0
            }
            if(nrow(off_2_pts) != 0) {
              freq_o2$Freq <- df_o_2$Freq[match(freq_o2$Var1, df_o_2$Var1)]
              freq_o2$Freq[is.na(freq_o2$Freq)] <- 0
            }
            
            #Final results
            return_pval = NA
            
            arr1 <- freq_a1$Freq
            arr2 <- freq_a2$Freq
            off1 <- freq_o1$Freq
            off2 <- freq_o2$Freq
            
            if(sum(off1 == 0) > 0 | sum(off2 == 0) > 0) {
              off1 <- off1 + 1
              off2 <- off2 + 1
            }
            
            arr1 <- arr1 / off1
            arr2 <- arr2 / off2
            
            arrDiff <- data.frame(arr1 - arr2)
            colnames(arrDiff) <- c("difference")
            
            #Scales the differences to be compatible with "arima"
            newDifference <- arrDiff$difference/sd(arrDiff$difference)
            if(!is.na(sum(newDifference)))  {
              ## Now do a naive test of the means similar to how we were doing it for the policing example
              ## PERFORMING THE ARIMA TEST!! ##
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
            
            # Naive p-value
            count1 = nrow(arr_1_pts)
            count2 = nrow(arr_2_pts)
            n = count1 + count2
            p = 0.5
            pval = NA
            
            if (count1 <= n/2) {
              pval = pbinom(count1, n, p) + 1 - pbinom(count2, n, p)
            } else {
              pval = pbinom(count2, n, p) + 1 - pbinom(count1, n, p)
            }
            
            nullStr_point_data$DATA[rowNum,] = c(k, i, j,
                                                 streetLengthInfo_null[[i]][[j]]$streetLength1,
                                                 streetLengthInfo_null[[i]][[j]]$streetLength2,
                                                 area1, area2, T, return_pval, pval)
            
            nullStr_point_data$ARR_IND_1[[rowNum]] = arr_1_ind
            nullStr_point_data$ARR_IND_2[[rowNum]] = arr_2_ind
            nullStr_point_data$OFF_IND_1[[rowNum]] = off_1_ind
            nullStr_point_data$OFF_IND_2[[rowNum]] = off_2_ind
            
            rowNum = rowNum + 1
          }
        }
      }
    }

    save(nullStr_point_data, file=paste0("../Output/nullGridInfo/nullData", 
          index, "_", k,".dat", sep=''))

}



# {
#     #the goal here is to load all the broken streets and then get the p-vals
#     #This is for ARRESTS
#     TwoBuf <- function(line,width,minEx){
#       Buf0 <- gBuffer(line,width=minEx,capStyle="SQUARE")
#       Buf1 <- gBuffer(line,width=width,capStyle="FLAT")
#       return(disaggregate(gDifference(Buf1,Buf0)))
#     }
# 
#     arimaTestNullBroken2new <- function(bufferWidth, i, longStrBroke) {
# 
#       buffIndex <- bufferWidth / 10
#       index2 = bufferWidth / 100
#       endResults <- vector(mode = "list", length = length(longStrBroke[[i]]))
#       #Filter my arrest and offense data
#       precIndex = nyc$Precinct[i]
#       data_arrest <- dplyr::filter(dataArrF, arrest_precinct == precIndex)
#       data_offense <- dplyr::filter(dataOffFiltered, precinct == precIndex)
# 
#       #Adding monthkey
#       data_arrest <- data_arrest %>%
#         dplyr::mutate(monthKey = as.numeric(paste0(data_arrest$month, data_arrest$year)))
#       data_offense <- data_offense %>%
#         dplyr::mutate(monthKey = as.numeric(paste0(data_offense$month, data_offense$year)))
# 
#         print(paste0("i: ", i))
#         for(j in 1:length(longStrBroke[[i]])) {
#           print(paste0("j: ", j))
#           endResults[[j]] = vector(mode = "list", length = length(longStrBroke[[i]][[j]]))
#           for(k in 1:length(longStrBroke[[i]][[j]])) {
#             endResults[[j]][[k]] = NULL #initialize to null
#             if(!is.null(longStrBroke[[i]][[j]][[k]])) {
# 
#               myStreet <- longStrBroke[[i]][[j]][[k]]$shorterStreet
# 
#               #Create the polygon buffer
#               myPolyBIG <- TwoBuf(line=myStreet,width=bufferWidth,minEx=0.0001)
#               myPolySMALL <- TwoBuf(line=myStreet,width=50,minEx=0.0001)
# 
# 
#               if(length(myPolyBIG@polygons) >= 2 & length(myPolySMALL@polygons) >= 2) {
# 
#               a_BIG_1 <- point.in.polygon(data_arrest$x_coord_cd, data_arrest$y_coord_cd,
#                                           myPolyBIG@polygons[[1]]@Polygons[[1]]@coords[,1],
#                                           myPolyBIG@polygons[[1]]@Polygons[[1]]@coords[,2])
#               a_SMALL_1 <- point.in.polygon(data_arrest$x_coord_cd, data_arrest$y_coord_cd,
#                                             myPolySMALL@polygons[[1]]@Polygons[[1]]@coords[,1],
#                                             myPolySMALL@polygons[[1]]@Polygons[[1]]@coords[,2])
#               a_BIG_2 <- point.in.polygon(data_arrest$x_coord_cd, data_arrest$y_coord_cd,
#                                           myPolyBIG@polygons[[2]]@Polygons[[1]]@coords[,1],
#                                           myPolyBIG@polygons[[2]]@Polygons[[1]]@coords[,2])
#               a_SMALL_2 <- point.in.polygon(data_arrest$x_coord_cd, data_arrest$y_coord_cd,
#                                             myPolySMALL@polygons[[2]]@Polygons[[1]]@coords[,1],
#                                             myPolySMALL@polygons[[2]]@Polygons[[1]]@coords[,2])
# 
#               o_BIG_1 <- point.in.polygon(data_offense$x_coord_cd, data_offense$y_coord_cd,
#                                           myPolyBIG@polygons[[1]]@Polygons[[1]]@coords[,1],
#                                           myPolyBIG@polygons[[1]]@Polygons[[1]]@coords[,2])
#               o_SMALL_1 <- point.in.polygon(data_offense$x_coord_cd, data_offense$y_coord_cd,
#                                             myPolySMALL@polygons[[1]]@Polygons[[1]]@coords[,1],
#                                             myPolySMALL@polygons[[1]]@Polygons[[1]]@coords[,2])
#               o_BIG_2 <- point.in.polygon(data_offense$x_coord_cd, data_offense$y_coord_cd,
#                                           myPolyBIG@polygons[[2]]@Polygons[[1]]@coords[,1],
#                                           myPolyBIG@polygons[[2]]@Polygons[[1]]@coords[,2])
#               o_SMALL_2 <- point.in.polygon(data_offense$x_coord_cd, data_offense$y_coord_cd,
#                                             myPolySMALL@polygons[[2]]@Polygons[[1]]@coords[,1],
#                                             myPolySMALL@polygons[[2]]@Polygons[[1]]@coords[,2])
#               #Getting the actual data points
#               a_points_B_1 <- data_arrest[c(which(a_BIG_1 > 0)), ]
#               a_points_B_2 <- data_arrest[c(which(a_BIG_2 > 0)), ]
#               a_points_S_1 <- data_arrest[c(which(a_SMALL_1 > 0)), ]
#               a_points_S_2 <- data_arrest[c(which(a_SMALL_2 > 0)), ]
# 
#               o_points_B_1 <- data_offense[c(which(o_BIG_1 > 0)), ]
#               o_points_B_2 <- data_offense[c(which(o_BIG_2 > 0)), ]
#               o_points_S_1 <- data_offense[c(which(o_SMALL_1 > 0)), ]
#               o_points_S_2 <- data_offense[c(which(o_SMALL_2 > 0)), ]
# 
#               #Getting into MONTH_YEAR format
#               df_a_B_1 <- data.frame(table(a_points_B_1$monthKey))
#               df_a_B_2 <- data.frame(table(a_points_B_2$monthKey))
#               df_a_S_1 <- data.frame(table(a_points_S_1$monthKey))
#               df_a_S_2 <- data.frame(table(a_points_S_2$monthKey))
# 
#               df_o_B_1 <- data.frame(table(o_points_B_1$monthKey))
#               df_o_B_2 <- data.frame(table(o_points_B_2$monthKey))
#               df_o_S_1 <- data.frame(table(o_points_S_1$monthKey))
#               df_o_S_2 <- data.frame(table(o_points_S_2$monthKey))
# 
#               freqB1 = freqB2 = freqS1 = freqS2 = freqOffB1 = freqOffB2 = freqOffS1 = freqOffS2 = monthYearKey
# 
#               if(nrow(a_points_B_1) != 0) {
#                 freqB1$Freq <- df_a_B_1$Freq[match(freqB1$Var1, df_a_B_1$Var1)]
#                 freqB1$Freq[is.na(freqB1$Freq)] <- 0
#               }
#               if(nrow(a_points_B_2) != 0) {
#                 freqB2$Freq <- df_a_B_2$Freq[match(freqB2$Var1, df_a_B_2$Var1)]
#                 freqB2$Freq[is.na(freqB2$Freq)] <- 0
#               }
#               if(nrow(a_points_S_1) != 0) {
#                 freqS1$Freq <- df_a_S_1$Freq[match(freqS1$Var1, df_a_S_1$Var1)]
#                 freqS1$Freq[is.na(freqS1$Freq)] <- 0
#               }
#               if(nrow(a_points_S_2) != 0) {
#                 freqS2$Freq <- df_a_S_2$Freq[match(freqS2$Var1, df_a_S_2$Var1)]
#                 freqS2$Freq[is.na(freqS2$Freq)] <- 0
#               }
# 
#               if(nrow(o_points_B_1) != 0) {
#                 freqOffB1$Freq <- df_o_B_1$Freq[match(freqOffB1$Var1, df_o_B_1$Var1)]
#                 freqOffB1$Freq[is.na(freqOffB1$Freq)] <- 0
#               }
#               if(nrow(o_points_B_2) != 0) {
#                 freqOffB2$Freq <- df_o_B_2$Freq[match(freqOffB2$Var1, df_o_B_2$Var1)]
#                 freqOffB2$Freq[is.na(freqOffB2$Freq)] <- 0
#               }
#               if(nrow(o_points_S_1) != 0) {
#                 freqOffS1$Freq <- df_o_S_1$Freq[match(freqOffS1$Var1, df_o_S_1$Var1)]
#                 freqOffS1$Freq[is.na(freqOffS1$Freq)] <- 0
#               }
#               if(nrow(o_points_S_2) != 0) {
#                 freqOffS2$Freq <- df_o_S_2$Freq[match(freqOffS2$Var1, df_o_S_2$Var1)]
#                 freqOffS2$Freq[is.na(freqOffS2$Freq)] <- 0
#               }
# 
#               #Final results
#               return_pval = 0
# 
#               arr1 <- freqB1$Freq - freqS1$Freq
#               arr2 <- freqB2$Freq - freqS2$Freq
#               off1 <- freqOffB1$Freq - freqOffS1$Freq
#               off2 <- freqOffB2$Freq - freqOffS2$Freq
# 
#               if(sum(off1 == 0) > 0 || sum(off2 == 0) > 0) {
#                 off1 <- off1 + 1
#                 off2 <- off2 + 1
#               }
# 
#               arr1 <- arr1 / off1
#               arr2 <- arr2 / off2
# 
#               arrDiff <- data.frame(arr1 - arr2)
#               colnames(arrDiff) <- c("difference")
# 
#               #Scales the differences to be compatible with "arima"
#               newDifference <- arrDiff$difference/sd(arrDiff$difference)
#               if(is.na(sum(newDifference))) {
#                 return_pval <- NA
#               }
# 
#               else {
#                 ## Now do a naive test of the means similar to how we were doing it for the policing example
#                 ## PERFORMING THE ARIMA TEST!! ##
#                 print("check1")
#                 myModel = arima(newDifference, order = c(1, 0, 0))
#                 print("check2")
#                 SE = sqrt(myModel$var.coef[2,2])
#                 EST = myModel$coef[2]
# 
#                 if (EST < 0) {
#                   PVALUE = as.numeric(2*pnorm(EST, mean=0, sd=SE))
#                 } else {
#                   PVALUE = as.numeric(2*(1 - pnorm(EST, mean=0, sd=SE)))
#                 }
# 
#                 return_pval = PVALUE
#               }
# 
#               endResults[[j]][[k]] <- list("pValue" = return_pval,
#                                             "ID" = longStrBroke[[i]][[j]][[k]]$uniqueID)
# 
#               }
#             }
#           }
#         }
# 
#       return(endResults)
#     }
# 
#     index <- as.numeric(Sys.getenv('SLURM_ARRAY_TASK_ID'))
#     b = index*10
#     c = index * 100
#     load(paste0("Output/streets", index, ".dat")) # this gets loaded as "longStrBroke"
#     pValInfoArrNew = vector(mode = "list", length = length(longStrBroke))
# 
#     for (i in 1:length(pValInfoArrNew)) {
#       pValInfoArrNew[[i]] <- arimaTestNullBroken2new(c, i, longStrBroke)
#     }
#     save(pValInfoArrNew, file=paste("/blue/jantonelli/emmett.kendall/JoeyProject/CrimeProject/OutputNew/pInfoArrNew",
#                                  index, ".dat", sep=''))
# 
#   }