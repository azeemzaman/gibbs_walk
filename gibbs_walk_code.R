library(mvtnorm)
library(ggplot2)

Sigma1 <- matrix(c(1, -.95, -.95, 1), nrow = 2)
Sigma2 <- matrix(c(1, .95, .95, 1), nrow = 2)
mu1 <- c(-1.4,0)
mu2 <- c(1.4,0)
leftXs <- rmvnorm(5000, mean = mu1, Sigma1)
rightXs <- rmvnorm(5000, mean = mu2, Sigma2)

dat <- as.data.frame(rbind(leftXs, rightXs))

base_p <- ggplot(dat) + geom_density2d(aes(x = V1, y = V2)) + xlim(-5, 5) + ylim(-4, 2.5)


calc_cond_params <- function(cur_val, cond_var){
  # cond_var = 1 if conditioning on x
  # con_var = 2 if conditioning on y
  mu1_c <- mu1[3 -cond_var] + 
    sqrt(Sigma1[3 -cond_var,3 - cond_var]/Sigma1[cond_var,cond_var])*(cur_val - mu1[cond_var])*Sigma1[1,2]
  sigma1_c <- (1 - Sigma1[1,2]^2)*Sigma1[3 - cond_var, 3 - cond_var]
  
  mu2_c <- mu2[3 -cond_var] + 
    sqrt(Sigma2[3 -cond_var,3 - cond_var]/Sigma2[cond_var,cond_var])*(cur_val - mu2[cond_var])*Sigma2[1,2]
  sigma2_c <- (1 - Sigma2[1,2]^2)*Sigma2[3 - cond_var, 3 - cond_var]
  return(c(mu1_c = mu1_c, mu2_c = mu2_c, sigma1_c = sigma1_c, sigma2_c = sigma2_c))
}

calc_cond_den <- function(cur_val, cond_var){
  cur_params <- calc_cond_params(cur_val, cond_var)
  if (cond_var == 1){
    domain <- seq(from = -4, to = 2.5, length.out = 100)
  } else {
    domain <- seq(from = -5, to = 5, length.out = 100)
  }
  d1 <- sapply(domain, function(x) {
    dnorm(x, mean = cur_params[1], sd = sqrt(cur_params[3]))
  })
  d2 <- sapply(domain, function(x) {
    dnorm(x, mean = cur_params[2], sd = sqrt(cur_params[4]))
  })
  tot <- cbind(domain, d1 + d2)
  return(tot)
}


add_to_plot <- function(cur_val, cond_var){
  den_vals <- data.frame(calc_cond_den(cur_val, cond_var))
  if (cond_var == 2){
    new_plot <- base_p + geom_hline(yintercept = cur_val) +
      geom_line(data = den_vals, aes(x = domain, y = V2 + cur_val))
  } else {
    new_plot <- base_p + geom_vline(xintercept = cur_val) +
      geom_path(data = den_vals, aes(x = V2 + cur_val, y = domain))
  }
  return(new_plot)
}
test_vals <- data.frame(calc_cond_den(1, 2))
plot(test_vals[,1], test_vals[,2])

base_p + geom_hline(yintercept = 1) +
  geom_line(data = data.frame(test_vals), aes(x = domain, y = V2 + 1))


make_gif <- function(curX, curY, n.iter = 20){
  for (iter in 1:n.iter){
    # update x
    # sample latent z
    x_plot <- add_to_plot(curY, 2) + geom_point(x = curX, y = curY)
    print(x_plot)
    z <- rbinom(1, 1, prob = 0.5) + 1
    x_params <- calc_cond_params(curY, 2)
    curX <- rnorm(1, mean = x_params[z], sd = sqrt(x_params[z + 2]))
    x_plot <- add_to_plot(curY, 2) + geom_point(x = curX, y = curY)
    print(x_plot)
    # update y
    y_plot <- add_to_plot(curX, 1) + geom_point(x = curX, y = curY)
    print(y_plot)
    y_params <- calc_cond_params(curX, 1)
    curY <- rnorm(1, mean = y_params[z], sd = sqrt(y_params[z + 2]))
    y_plot <- add_to_plot(curX, 1) + geom_point(x = curX, y = curY)
    print(y_plot)
  }
}
