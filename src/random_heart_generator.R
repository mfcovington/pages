circle <- function(center = c(0, 0), diameter = 1, npoints = 100){
    # adapted from :
    # http://stackoverflow.com/questions/6862742/draw-a-circle-with-ggplot2
    r = diameter / 2
    tt <- seq(0, 2 * pi, length.out = npoints)
    xx <- center[1] + r * cos(tt)
    yy <- center[2] + r * sin(tt)
    return(data.frame(x = xx, y = yy))
}

diam <- function(center = c(0, 0), width = 2) {
    r <- width / 2
    xx <- center[1]
    yy <- center[2]
    return(data.frame(x = c(xx,     xx + r, xx,     xx - r),
                      y = c(yy + r, yy,     yy - r, yy)))
}

hearts <- function (n = 5, up = TRUE, free.love = FALSE) {
  if (n > 100 && !free.love)
    return("That's way too many hearts for you to handle.")

  cardio <- ggplot(mapping = aes(x, y))

  for (i in 1:n) {
    center     <- rnorm(2)
    width      <- ifelse(up, abs(rnorm(1)), rnorm(1))
    fill.color <- c(rainbow(99), "black")[sample(1:100, 1)]

    ventricles  <- diam(center = center, width = width)
    aorta.left  <- circle(center   = (center + c(-width / 4, width / 4)),
                          diameter = sqrt(2 * (width / 2)^2))
    aorta.right <- circle(center   = (center + c(width / 4, width / 4)),
                          diameter = sqrt(2 * (width / 2)^2))

    cardio <- cardio +
      geom_polygon(data = ventricles,  fill = fill.color) +
      geom_polygon(data = aorta.left,  fill = fill.color) +
      geom_polygon(data = aorta.right, fill = fill.color)
  }

  cardio <- cardio +
    coord_fixed(ratio = 1) +
    theme(panel.grid = element_blank(),
          title      = element_blank(),
          rect       = element_blank(),
          text       = element_blank(),
          line       = element_blank())

  print(cardio)
}
