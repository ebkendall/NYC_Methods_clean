library(sp); library(sf); library(rgeos); library(raster)

load("../Data/indexList_MAIN.RData")            # indexList_MAIN
load("../Data/streetsByPrec.RData")             # streetsByPrec
load("../Data/nycSub.RData")
load("../Data/originalBorderLines_reformat.RData")
load("../Data/totalStreetBuffInfo_ORIG.RData")

# Buffer Splitting function
bufferSplit_null <- function(myLine, buf) {
  
  temp = myLine
  
  temp_coords = temp@lines[[1]]@Lines[[1]]@coords
  
  if(sum(temp_coords[1,] == temp_coords[nrow(temp_coords), ]) == 2) {
    unique_coords = matrix(temp_coords[!(duplicated(temp_coords)|duplicated(temp_coords, fromLast=TRUE))],
                           nrow=2,ncol=2)
    
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

totalStreetBuffInfo_NEW <- vector(mode = "list", length = 13)
for(k in 2:13) {
  totalStreetBuffInfo_NEW[[k]] = vector(mode = "list", length = 164)
  for(i in indexList_MAIN) {
    print(paste0(k, ", ", i))
    # New buffer
    temp = bufferSplit_null(originalBorderLines_reformat[[i]]$borderLine, k*100)
    
    nyc_small = nycSub[c(totalStreetBuffInfo_ORIG[[k]][[i]]$precInd1,
                         totalStreetBuffInfo_ORIG[[k]][[i]]$precInd2), ]
    tempOverlap = gIntersection(temp, nyc_small, byid = T)
    poly_order = tempOverlap@plotOrder[1:2]

    int_1 = gIntersection(tempOverlap[poly_order[1], ], nycSub[totalStreetBuffInfo_ORIG[[k]][[i]]$precInd1, ])
    int_2 = gIntersection(tempOverlap[poly_order[1], ], nycSub[totalStreetBuffInfo_ORIG[[k]][[i]]$precInd2, ])

    if(is.null(int_1)) {
      int_1 = 0
    } else {
      int_1 = int_1@polygons[[1]]@area
    }

    if(is.null(int_2)) {
      int_2 = 0
    } else {
      int_2 = int_2@polygons[[1]]@area
    }

    if(int_1 < int_2) {
      temp = poly_order[1]
      poly_order[1] = poly_order[2]
      poly_order[2] = temp
    }

    poly1 = tempOverlap[poly_order[1], ]
    poly2 = tempOverlap[poly_order[2], ]

    poly_ind1 = poly1@polygons[[1]]@plotOrder[1]
    poly_ind2 = poly2@polygons[[1]]@plotOrder[1]

    area1 = poly1@polygons[[1]]@Polygons[[poly_ind1]]@area
    area2 = poly2@polygons[[1]]@Polygons[[poly_ind2]]@area

    s1_objects = gIntersection(poly1, streetsByPrec[[totalStreetBuffInfo_ORIG[[k]][[i]]$precInd1]])
    s1 = gLength(s1_objects)
    s2_objects = gIntersection(poly2, streetsByPrec[[totalStreetBuffInfo_ORIG[[k]][[i]]$precInd2]])
    s2 = gLength(s2_objects)
    
    totalStreetBuffInfo_NEW[[k]][[i]] = list("prec1" = totalStreetBuffInfo_ORIG[[k]][[i]]$prec1,
                                             "prec2" = totalStreetBuffInfo_ORIG[[k]][[i]]$prec2,
                                             "precInd1" = totalStreetBuffInfo_ORIG[[k]][[i]]$precInd1,
                                             "precInd2" = totalStreetBuffInfo_ORIG[[k]][[i]]$precInd2,
                                             "area1" = area1, "area2" = area2,
                                             "streetLength1" = s1, "streetLength2" = s2,
                                             "centerLine" = originalBorderLines_reformat[[i]]$borderLine,
                                             "poly1" = poly1, "poly2" = poly2,
                                             "poly_ind1" = poly_ind1,
                                             "poly_ind2" = poly_ind2)
  }
}

save(totalStreetBuffInfo_NEW, file = "../Data/totalStreetBuffInfo_NEW.RData")

# load('../Data/totalStreetBuffInfo_NEW.RData')
# load('../Data/totalStreetBuffInfo_ORIG.RData')
# 
# pdf(paste0("../Output_tree/Plots/newBorderCompare2.pdf"))
# par(mfrow=c(3,2))
# for(i in indexList_MAIN) {
#   for (buff in 2:13) {
#     print(i)
#     plot(totalStreetBuffInfo_NEW[[buff]][[i]]$poly1, main = paste0("New: ", i), border = "green")
#     plot(totalStreetBuffInfo_NEW[[buff]][[i]]$poly2, border = "red", add = T)
#     plot(nycSub[c(totalStreetBuffInfo_NEW[[buff]][[i]]$precInd1, totalStreetBuffInfo_NEW[[buff]][[i]]$precInd2), ], add = T)
#     
#     plot(totalStreetBuffInfo_ORIG[[buff]][[i]]$buffer, main = paste0("Old: ", i), border = "green", lwd = 2)
#     # plot(totalStreetBuffInfo_ORIG[[1]][[i]]$buffer, border = "red", add = T)
#     plot(nycSub[c(totalStreetBuffInfo_ORIG[[buff]][[i]]$precInd1, totalStreetBuffInfo_ORIG[[buff]][[i]]$precInd2), ], add = T)
#   }
# }
# dev.off()