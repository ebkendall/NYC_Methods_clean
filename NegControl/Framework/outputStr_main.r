library(sp); library(sf); library(rgeos); library(raster)

load("../Data/indexList_MAIN.RData")            # indexList_MAIN
load("../Data/streetsByPrec.RData")             # streetsByPrec
load("../Data/nycSub.RData")

index <- as.numeric(Sys.getenv('SLURM_ARRAY_TASK_ID'))
load(paste0("/blue/jantonelli/emmett.kendall/JoeyProject/Crime10-31-2021/NullStreetInfo/streets", index, ".dat"))  # longStrBroke
# Buffer Splitting function
bufferSplit_null <- function(myLine, buf) {
  
  temp = myLine
  
  temp_coords = temp@lines[[1]]@Lines[[1]]@coords
  
  if(sum(temp_coords[1,] == temp_coords[nrow(temp_coords), ]) == 2) {
    dup = temp_coords[!(duplicated(temp_coords)|duplicated(temp_coords, fromLast=TRUE))]
    unique_coords = matrix(dup, nrow=length(dup)/2)
    
    ind1 = which(temp_coords[,"x"] == unique_coords[1,1] & temp_coords[,"y"] == unique_coords[1,2])
    
    coord_p1 = temp_coords[(ind1+1):nrow(temp_coords), ]
    coord_p2 = temp_coords[1:ind1, ]
    coord_sub = rbind(coord_p1, coord_p2)
    temp@lines[[1]]@Lines[[1]]@coords = coord_sub
  }
  
  tempBuff = gBuffer(temp, width = buf)
  topCoord = temp@lines[[1]]@Lines[[1]]@coords[1:2,]
  botCoord = tail(temp@lines[[1]]@Lines[[1]]@coords, 2)
  
  a = atan2((topCoord[1,2] - topCoord[2,2]), (topCoord[1,1] - topCoord[2,1]))
  b = atan2((botCoord[2,2] - botCoord[1,2]),(botCoord[2,1] - botCoord[1,1]))
  
  top_x = topCoord[1,1] + (buf + 50) * cos(a)
  top_y = topCoord[1,2] + (buf + 50) * sin(a)
  
  temp@lines[[1]]@Lines[[1]]@coords = rbind(c(top_x, top_y), temp@lines[[1]]@Lines[[1]]@coords)
  
  bot_x = botCoord[2,1] + (buf + 50) * cos(b)
  bot_y = botCoord[2,2] + (buf + 50) * sin(b)
  
  temp@lines[[1]]@Lines[[1]]@coords = rbind(temp@lines[[1]]@Lines[[1]]@coords, c(bot_x, bot_y))
  
  tempBuff2 = gBuffer(temp, width=30)
  tempDiff = disaggregate(gDifference(tempBuff, tempBuff2))
  
  return(tempDiff)
  
}

for (k in 1:77) {
  streetLengthInfo_null <- vector(mode = "list", length = length(longStrBroke[[k]]))
  print(k)
  
  for (i in 1:length(longStrBroke[[k]])) {
    print(paste0(i, " out of ", length(longStrBroke[[k]])))
    streetLengthInfo_null[[i]] <- vector(mode = "list", length = length(longStrBroke[[k]][[i]]))
    for(j in 1:length(longStrBroke[[k]][[i]])) {
      if(!is.null(longStrBroke[[k]][[i]][[j]])) {
        
        temp = bufferSplit_null(longStrBroke[[k]][[i]][[j]]$shorterStreet, index*100)
        temp@proj4string = nycSub@proj4string
        # Check to see if buffer is completely within a precinct
        if(gContains(nycSub[k,], temp)) {
          # Check if the buffer was split properly
          if(length(temp@polygons) == 2) {
            temp_strt = gIntersection(temp, streetsByPrec[[k]], byid = T)
            streetLength1 = streetLength2 = NA
            # Check if there exists streets on both sides
            if(!is.null(temp_strt)) {
              if(length(temp_strt@lines) == 2){
                streetLength1 <- gLength(temp_strt[1,]) # corresponds to temp@polygons[[1]]
                streetLength2 <- gLength(temp_strt[2,]) # corresponds to temp@polygons[[2]]
              }
              
              if(is.na(streetLength1)) {streetLength1 = 0}
              if(is.na(streetLength2)) {streetLength2 = 0}
              totalLength = streetLength1 + streetLength2
              
              if(totalLength > 0) {
                streetLengthInfo_null[[i]][[j]] <- list("streetLength1" = streetLength1,
                                                        "streetLength2" = streetLength2,
                                                        "buffer" = temp)
              } else {
                streetLengthInfo_null[[i]][[j]] = NA
              }
            } else {
              streetLengthInfo_null[[i]][[j]] = NA
            }
          } else {
            streetLengthInfo_null[[i]][[j]] = NA
          }
        } else {
          streetLengthInfo_null[[i]][[j]] = NA
        }
      } else {
        streetLengthInfo_null[[i]][[j]] = NA
      }
    }
  }
  
  
  
  save(streetLengthInfo_null, 
       file=paste0("../Data/OutputStrInfo_realData/strInfo_", index, "_", k, ".dat", sep=''))
  
}

