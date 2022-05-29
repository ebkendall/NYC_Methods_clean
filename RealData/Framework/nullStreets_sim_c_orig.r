library(sp); library(sf); library(rgeos); library(raster)

load("../Data/nycSub.RData")
load("../Data/ind_prec_df.rda")
load("../Data/indexList_MAIN.RData")
load("../Data/totalStreetBuffInfo_ORIG.RData")
load('../Data/dataArr_sub.rda') # dataArr_sub
load('../Data/dataOff_sub.rda') # dataOff_sub
Dir = '../Output/origGridInfo/'
print(Dir)

for (k in 2:13) {

  sim_orig <- list(DATA = data.frame("area1" = rep(NA,164), "area2" = rep(NA,164), 
                                     "streets1" = rep(NA, 164), "streets2" = rep(NA, 164)),
                   ARR_IND_1 = vector(mode = 'list', length = 164),
                   ARR_IND_2 = vector(mode = 'list', length = 164),
                   OFF_IND_1 = vector(mode = 'list', length = 164),
                   OFF_IND_2 = vector(mode = 'list', length = 164))
    
  for (i in indexList_MAIN) {
    
    prec_ind_1 = which(nycSub$Precinct == ind_prec_df$prec1[i])
    prec_ind_2 = which(nycSub$Precinct == ind_prec_df$prec2[i])

    nyc_small = nycSub[c(prec_ind_1, prec_ind_2), ]

    # Arrest Info
    t_grid1 = data.frame(dataArr_sub[dataArr_sub$arrest_precinct==ind_prec_df$prec1[i], c("x_coord_cd", "y_coord_cd", "main_ind")],
                         "prec" = rep(1, nrow(dataArr_sub[dataArr_sub$arrest_precinct==ind_prec_df$prec1[i],])),
                         "o_ind" = 1:nrow(dataArr_sub[dataArr_sub$arrest_precinct==ind_prec_df$prec1[i],]))
    t_grid2 = data.frame(dataArr_sub[dataArr_sub$arrest_precinct==ind_prec_df$prec2[i], c("x_coord_cd", "y_coord_cd", "main_ind")],
                         "prec" = rep(2, nrow(dataArr_sub[dataArr_sub$arrest_precinct==ind_prec_df$prec2[i],])),
                         "o_ind" = 1:nrow(dataArr_sub[dataArr_sub$arrest_precinct==ind_prec_df$prec2[i],]))
    
    # Offense Info
    o_grid1 = data.frame(dataOff_sub[dataOff_sub$precinct==ind_prec_df$prec1[i], c("x_coord_cd", "y_coord_cd", "main_ind")],
                         "prec" = rep(1, nrow(dataOff_sub[dataOff_sub$precinct==ind_prec_df$prec1[i],])),
                         "o_ind" = 1:nrow(dataOff_sub[dataOff_sub$precinct==ind_prec_df$prec1[i],]))
    o_grid2 = data.frame(dataOff_sub[dataOff_sub$precinct==ind_prec_df$prec2[i], c("x_coord_cd", "y_coord_cd", "main_ind")],
                         "prec" = rep(2, nrow(dataOff_sub[dataOff_sub$precinct==ind_prec_df$prec2[i],])),
                         "o_ind" = 1:nrow(dataOff_sub[dataOff_sub$precinct==ind_prec_df$prec2[i],]))

    colnames(t_grid1) = colnames(t_grid2) = colnames(o_grid1) = colnames(o_grid2) = c("x", "y", "main_ind", "prec", "o_ind")

    print(paste0("index ", i, " of 164"))

    tempOverlap = gIntersection(totalStreetBuffInfo_ORIG[[k]][[i]]$buffer, nyc_small, byid = T)
    poly_order = tempOverlap@plotOrder[1:2]

    int_1 = gIntersection(tempOverlap[poly_order[1], ], nycSub[prec_ind_1, ])
    int_2 = gIntersection(tempOverlap[poly_order[1], ], nycSub[prec_ind_2, ])

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

    # Collecting Arrest counts for Precinct 1 across both buffers
    arr1_1 = point.in.polygon(t_grid1[,1], t_grid1[,2],
                          poly1@polygons[[1]]@Polygons[[poly_ind1]]@coords[,1],
                          poly1@polygons[[1]]@Polygons[[poly_ind1]]@coords[,2])
    arr2_1 = point.in.polygon(t_grid1[,1], t_grid1[,2],
                          poly2@polygons[[1]]@Polygons[[poly_ind2]]@coords[,1],
                          poly2@polygons[[1]]@Polygons[[poly_ind2]]@coords[,2])
    # Collecting Arrest counts for Precinct 2 across both buffers
    arr1_2 = point.in.polygon(t_grid2[,1], t_grid2[,2],
                              poly1@polygons[[1]]@Polygons[[poly_ind1]]@coords[,1],
                              poly1@polygons[[1]]@Polygons[[poly_ind1]]@coords[,2])
    arr2_2 = point.in.polygon(t_grid2[,1], t_grid2[,2],
                              poly2@polygons[[1]]@Polygons[[poly_ind2]]@coords[,1],
                              poly2@polygons[[1]]@Polygons[[poly_ind2]]@coords[,2])
    # Collecting Offense counts for Precinct 1 across both buffers
    off1_1 = point.in.polygon(o_grid1[,1], o_grid1[,2],
                              poly1@polygons[[1]]@Polygons[[poly_ind1]]@coords[,1],
                              poly1@polygons[[1]]@Polygons[[poly_ind1]]@coords[,2])
    off2_1 = point.in.polygon(o_grid1[,1], o_grid1[,2],
                              poly2@polygons[[1]]@Polygons[[poly_ind2]]@coords[,1],
                              poly2@polygons[[1]]@Polygons[[poly_ind2]]@coords[,2])
    # Collecting Offense counts for Precinct 2 across both buffers
    off1_2 = point.in.polygon(o_grid2[,1], o_grid2[,2],
                              poly1@polygons[[1]]@Polygons[[poly_ind1]]@coords[,1],
                              poly1@polygons[[1]]@Polygons[[poly_ind1]]@coords[,2])
    off2_2 = point.in.polygon(o_grid2[,1], o_grid2[,2],
                              poly2@polygons[[1]]@Polygons[[poly_ind2]]@coords[,1],
                              poly2@polygons[[1]]@Polygons[[poly_ind2]]@coords[,2])
    
    # If any of the items are greater than 0 then we want to add pointwise
    arrest1_total = arr1_1 + arr2_1
    arrest2_total = arr1_2 + arr2_2
    offense1_total = off1_1 + off2_1
    offense2_total = off1_2 + off2_2
    
    arr_1_ind = t_grid1$main_ind[which(arrest1_total > 0)]
    arr_2_ind = t_grid2$main_ind[which(arrest2_total > 0)]
    
    off_1_ind = o_grid1$main_ind[which(offense1_total > 0)]
    off_2_ind = o_grid2$main_ind[which(offense2_total > 0)]

    s1 = totalStreetBuffInfo_ORIG[[k]][[i]]$streetLength1
    s2 = totalStreetBuffInfo_ORIG[[k]][[i]]$streetLength2

    sim_orig$DATA[i,] = c(area1, area2, s1, s2)
    sim_orig$ARR_IND_1[[i]] = arr_1_ind
    sim_orig$ARR_IND_2[[i]] = arr_2_ind
    sim_orig$OFF_IND_1[[i]] = off_1_ind
    sim_orig$OFF_IND_2[[i]] = off_2_ind
  }

  save(sim_orig, file = paste0(Dir, 'sim_orig_', k, '.dat'))
}
