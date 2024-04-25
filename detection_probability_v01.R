################################################################
# write function to resample quadrats
quad_resamp <- function(quad_occupancy_vec, iterations){
  dframe.resamp <- matrix(nrow = iterations, ncol = length(quad_occupancy_vec))
  for(i in 1:iterations){
    dframe.resamp[i,] <- quad_occupancy_vec[sample(1:length(quad_occupancy_vec))]
    dframe.resamp[i,] <- cumsum(dframe.resamp[i,])
    dframe.resamp[i,] <- ifelse(dframe.resamp[i,] == 0, 0, 1)}
  
  dframe.resamp.prob <- data.frame(seq(1:length(quad_occupancy_vec)),
                                   apply(dframe.resamp, 2, mean),
                                   apply(dframe.resamp, 2, sd))
  colnames(dframe.resamp.prob) <- c("quad_no","detection_prob","stdev")
  dframe.resamp.prob <- rbind(c(0,0,0), dframe.resamp.prob)
}

################################################################
# simulate vectors with low or high occupancy
set.seed(1); vec.occup.low <- rbinom(24, 1, 0.2)
set.seed(1); vec.occup.high <- rbinom(24, 1, 0.8)

# resample quadrats to create detection curves
df.detect.low <- quad_resamp(vec.occup.low, 1000)
df.detect.high <- quad_resamp(vec.occup.high, 1000)

################################################################
# plot detection curves
plot(df.detect.high$quad_no,
     df.detect.high$detection_prob,
     type = "l",
     col = "blue", xlab = "quad count", ylab = "detection prob")

lines(df.detect.low$quad_no,
      df.detect.low$detection_prob,
      col = "red", type = "l")

legend("bottomright", legend=c("high", "low"),
       col=c("blue", "red"), lty = 1)

################################################################
