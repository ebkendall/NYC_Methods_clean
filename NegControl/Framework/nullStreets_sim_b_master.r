library(sp); library(sf); library(rgeos); library(raster)

load('../Data/nycSub.RData')
load("../Data/treesByPrec.RData")    # gridWithin_prec treesByPrec

# Iterate through each buffer width
for (index in 2:13) {
  
    k = as.numeric(Sys.getenv('SLURM_ARRAY_TASK_ID'))
    set.seed(k)

    prec_num = nycSub$Precinct[k]

    print(paste0("Buffer: ", index, ", Precinct: ", k))

    l = 1000 # arbitrary start length

    nullStr_point_data <- list(DATA = data.frame("precinct" = rep(-1,l), "indigo" = rep(-1,l), "juliet" = rep(-1,l),
                                      "count1" = rep(-1,l), "count2" = rep(-1,l),
                                      "streets1" = rep(-1,l), "streets2" = rep(-1,l),
                                      "area1" = rep(-1,l), "area2" = rep(-1,l), "tStat_area" = rep(NA,l), "naivePVal" = rep(-1,l),
                                      "totLength" = rep(-1,l), "splitProper" = rep(T,l), "tStat" = rep(NA,l)),
                              GRID_IND_1 = vector(mode = 'list', length = l),
                              GRID_IND_2 = vector(mode = 'list', length = l))
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

            s1 = streetLengthInfo_null[[i]][[j]]$streetLength1
            s2 = streetLengthInfo_null[[i]][[j]]$streetLength2

            p1 = point.in.polygon(treesByPrec[[k]][,1], treesByPrec[[k]][,2],
                                  poly1@Polygons[[1]]@coords[,1], poly1@Polygons[[1]]@coords[,2])
            p2 = point.in.polygon(treesByPrec[[k]][,1], treesByPrec[[k]][,2],
                                  poly2@Polygons[[1]]@coords[,1], poly2@Polygons[[1]]@coords[,2])

            t1 = count1 = length(which(p1 > 0)) # now I need to access their index
            t2 = count2 = length(which(p2 > 0))

            vals = c(t1,s1,t2,s2)
            if(sum(vals == 0) > 0) {
              if(vals[2] == 0 | vals[4] == 0) {
                vals = vals+1
              } else {
                vals[1] = vals[1] + 1
                vals[3] = vals[3] + 1
              }
            } 

            tStat = tStat_a = NA

            # Want division to be large / small (streets)
            if ((vals[1]/vals[2]) > (vals[3]/vals[4])) {
              tStat = (vals[1]/vals[2]) / (vals[3]/vals[4])
            } else {
              tStat = (vals[3]/vals[4]) / (vals[1]/vals[2])
            }

            # Want division to be large / small (area)
            if ((vals[1]/area1) > (vals[3]/area2)) {
              tStat_a = (vals[1]/area1) / (vals[3]/area2)
            } else {
              tStat_a = (vals[3]/area2) / (vals[1]/area1)
            }

            n = count1 + count2
            p = 0.5
            pval = NA

            if (count1 <= n/2) {
              pval = pbinom(count1, n, p) + 1 - pbinom(count2, n, p)
            } else {
              pval = pbinom(count2, n, p) + 1 - pbinom(count1, n, p)
            }
            
            nullStr_point_data$DATA[rowNum,] = c(k, i, j, t1, t2,
                                        streetLengthInfo_null[[i]][[j]]$streetLength1,
                                        streetLengthInfo_null[[i]][[j]]$streetLength2,
                                        area1, area2, tStat_a, pval,
                                        streetLengthInfo_null[[i]][[j]]$totalLength, T, tStat)
            
            nullStr_point_data$GRID_IND_1[[rowNum]] = which(p1 > 0)
            nullStr_point_data$GRID_IND_2[[rowNum]] = which(p2 > 0)
            
            rowNum = rowNum + 1
          }
        }
      }
    }

    save(nullStr_point_data, file=paste0("../Output_tree/nullGridInfo/nullData", 
          index, "_", k,".dat", sep=''))

}
