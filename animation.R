library(ggplot2)
library(gganimate)
library(dplyr)
library(viridis)
library(gifski)
# Define your data
x <- c(10, 15, 20, 24, 30, 34, 40, 45, 48, 50, 58, 60, 64)
y <- c(115.6, 157.2, 189.2, 220.8, 253.8, 269.2, 284.8, 285.0, 277.4,
       269.2, 244.2, 231.4, 180.4)

# Initialize parameters
TH.O <- c(-15, 16, -0.20, 50)
x.seq <- seq(min(x), max(x), length = 1000)
results <- data.frame()

n <- length(x)
sum.y <- sum(y)
sum.x <- sum(x)
sum.x2 <- sum(x^2)
sum.x3 <- sum(x^3)
sum.x4 <- sum(x^4)
sum.xy <- sum(x*y)
sum.x2y <- sum((x^2)*y)
sum.x3y <- sum((x^3)*y)

TH.N <- rep(0, 4)
TH.O <- c(-15, 13, -0.22, 50)

r <- 1
repeat {
  cat("Number of iteration\n", r, "\n")
  cat("Estimates\n", TH.O, "\n\n\n")

  y.seq <- TH.O[1] + TH.O[2] * x.seq + TH.O[3] * x.seq^2
  results <- rbind(results, data.frame(x = x.seq, y = y.seq, iter = r))

  alpha <- TH.O[1]
  beta <- TH.O[2]
  theta <- TH.O[3]
  sigma2 <- TH.O[4]

  SSR <- sum((y - alpha - beta * x - theta * x^2)^2)
  G.a <- (sum.y - n * alpha - sum.x * beta - sum.x2 * theta) / sigma2
  G.b <- (sum.xy - sum.x * alpha - sum.x2 * beta - sum.x3 * theta) / sigma2
  G.t <- (sum.x2y - sum.x2 * alpha - sum.x3 * beta - sum.x4 * theta) / sigma2
  G.s2 <- -n / (2 * sigma2) + (1 / (2 * (sigma2^2))) * SSR

  H.aa <- -n / sigma2
  H.ab <- -sum.x / sigma2
  H.at <- -sum.x2 / sigma2
  H.as2 <- (-1 / sigma2^2) * (sum.y - n * alpha - sum.x * beta - sum.x2 * theta)
  H.bb <- -sum.x2 / sigma2
  H.bt <- -sum.x3 / sigma2
  H.bs2 <- (-1 / sigma2^2) * (sum.xy - sum.x * alpha - sum.x2 * beta - sum.x3 * theta)
  H.tt <- -sum.x4 / sigma2
  H.ts2 <- (-1 / sigma2*2) * (sum.x2y - sum.x2 * alpha - sum.x3 * beta - sum.x4 * theta)
  H.s2s2 <- (n / 2 * (sigma2^2)) - (1 / (sigma2^3)) * SSR

  G <- c(G.a, G.b, G.t, G.s2)
  H <- matrix(c(H.aa, H.ab, H.at, H.as2,
                H.ab, H.bb, H.bt, H.bs2,
                H.at, H.bt, H.tt, H.ts2,
                H.as2, H.bs2, H.ts2, H.s2s2),
              ncol = 4, byrow = TRUE)
  TH.N <- TH.O - solve(H) %*% c(G)

  if (abs(TH.N[1] - TH.O[1]) < 0.0001 && abs(TH.N[2] - TH.O[2]) < 0.0001 &&
      abs(TH.N[3] - TH.O[3]) < 0.0001 && abs(TH.N[4] - TH.O[4]) < 0.00001) {
    break
  } else {
    TH.O <- TH.N
  }

  r <- r + 1
}

cat("Final estimates\n", TH.N, "\n\n")
y.seq <- TH.N[1] + TH.N[2] * x.seq + TH.N[3] * x.seq^2
results <- rbind(results, data.frame(x = x.seq, y = y.seq, iter = r))

# Plot using gganimate with viridis color palette and high contrast colors
p <- ggplot() +
  geom_point(aes(x = x, y = y), color = 'black', size = 3) +
  geom_line(data = results, aes(x = x, y = y, group = iter, color = as.factor(iter)), size = 1) +
  scale_color_viridis_d(option = "plasma") +
  labs(x = "Angle in degrees", y = "Distance traveled in feet", color = "Iteration") +
  transition_states(iter, transition_length = 2, state_length = 1) +
  ease_aes('linear') +
  theme_minimal(base_size = 15)

# Save the animation as a larger GIF
animate(p, nframes = 100, fps = 10, width = 800, height = 600, renderer = gifski_renderer("animation.gif"))
