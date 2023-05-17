library(sp); library(sf); library(rgeos); library(raster)

load("../Data/nycSub.RData")
load("../Data/ind_prec_df.rda")
load("../Data/indexList_MAIN.RData")
load("../Data/totalStreetBuffInfo_NEW.RData")
load("../Data/treesByPrec.RData")
load("../Data/streetsByPrec.RData")
Dir = '../Output_tree/origGridInfo/'
print(Dir)

for (k in 2:13) {

  sim_orig <- list(DATA = data.frame( "count1" = rep(-1,164), "count2" = rep(-1,164),
                                      "streets1" = rep(-1,164), "streets2" = rep(-1,164),
                                      "area1" = rep(-1,164), "area2" = rep(-1,164), "tStat_area" = rep(NA,164), "naivePVal" = rep(-1,164),
                                      "totLength" = rep(-1,164), "tStat" = rep(NA,164)),
                              GRID_IND_1 = vector(mode = 'list', length = 164),
                              GRID_IND_2 = vector(mode = 'list', length = 164))
    
  for (i in indexList_MAIN) {
    
    prec_ind_1 = which(nycSub$Precinct == ind_prec_df$prec1[i])
    prec_ind_2 = which(nycSub$Precinct == ind_prec_df$prec2[i])

    poly1 = totalStreetBuffInfo_NEW[[k]][[i]]$poly1
    poly2 = totalStreetBuffInfo_NEW[[k]][[i]]$poly2

    poly_ind1 = totalStreetBuffInfo_NEW[[k]][[i]]$poly_ind1
    poly_ind2 = totalStreetBuffInfo_NEW[[k]][[i]]$poly_ind2

    area1 = totalStreetBuffInfo_NEW[[k]][[i]]$area1
    area2 = totalStreetBuffInfo_NEW[[k]][[i]]$area2

    # Collecting Tree counts for Precinct 1 across both buffers
    p1 = point.in.polygon(treesByPrec[[prec_ind_1]][,1], treesByPrec[[prec_ind_1]][,2],
                          poly1@polygons[[1]]@Polygons[[poly_ind1]]@coords[,1],
                          poly1@polygons[[1]]@Polygons[[poly_ind1]]@coords[,2])
    p2 = point.in.polygon(treesByPrec[[prec_ind_2]][,1], treesByPrec[[prec_ind_2]][,2],
                          poly2@polygons[[1]]@Polygons[[poly_ind2]]@coords[,1],
                          poly2@polygons[[1]]@Polygons[[poly_ind2]]@coords[,2])
    
    t1 = count1 = length(which(p1 > 0)) 
    t2 = count2 = length(which(p2 > 0))
    
    s1 = totalStreetBuffInfo_NEW[[k]][[i]]$streetLength1
    s2 = totalStreetBuffInfo_NEW[[k]][[i]]$streetLength2
    
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

    sim_orig$DATA[i,] = c(count1, count2, s1, s2, area1, area2, tStat_a, pval, 
                          s1 + s2, tStat)
    sim_orig$GRID_IND_1[[i]] = which(p1 > 0)
    sim_orig$GRID_IND_2[[i]] = which(p2 > 0)
  }

  save(sim_orig, file = paste0(Dir, 'sim_orig_', k, '.dat'))
}
