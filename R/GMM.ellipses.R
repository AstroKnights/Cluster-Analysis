# Function to generate data frame for producing ellipses (DM)
GMM.ellipses <- function(mclustobj,level=0.95){
  nms <- rownames(mclustobj$parameters$mean)
  n <- length(nms)
  grid <- expand.grid(x = 1:n, y = 1:n)
  grid <- subset(grid, x < y)
  grid <- cbind(grid[, 2], grid[, 1])
  plyr::ldply(1:nrow(grid), function(i){
        coords <- as.numeric(grid[i, ])
        
        centers <- mclustobj$parameters$mean[coords, ]  
        vars <- mclustobj$parameters$variance$sigma[coords, coords, ]
        ell <- plyr::ldply(1:ncol(centers), function(cluster){
          data.frame(ellipse::ellipse(vars[,,cluster], centre = centers[, cluster],
                                      level = level), classification = as.factor(cluster))
        })
        names(ell) <- c('y', 'x', 'classification')
        data.frame(xvar = nms[coords[1]], yvar = nms[coords[2]], ell)
  })
}