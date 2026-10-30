hexamap <- function(data,                           #matrix: age as rows, period as columns
                                first_age,                      #integer: first age in the data
                                first_period,                   #integer: first period in the data
                                interval,                       #integer: data intervals
                                first_age_isoline = NULL,       #integer: first age isoline (default = first age)
                                first_period_isoline = NULL,    #integer: first period isoline (deault = first period)
                                isoline_interval = NULL,        #integer: intervals between isolines
                                colorbar_range = NULL,          #vector: lower and upper limits (default=c(min(data), max(data))
                                colorbar_scale = "Normal",      #string: how teh colors are scaled, either "Normal", "Log", "Percentile"
                                colorbar_width = 0.2,
                                colorbar_height = 0.25,
                                title_text = "Hexagonal Heatmap",
                                subtitle_text = "",
                                color_map = NULL,               #vector: colors from low to high (deault = jet)
                                line_width = .5,                #scalar: isoline line widths (default = 0.5)
                                line_color = "grey",            #string: line color name or code (default = gray)
                                label_size = .8,                #scalar: font size for the isoline and colorbar labels
                                label_color = "black",          #string: label color name or code (default = black)
                                scale_units = "Rate",           #string: text displayed above the colorbar (default = Rate)
                                n_rate_digits = 2,
                                zoom_factor = 1,
                                wrap_cohort_labels = TRUE){     #boolean: whether to wrap the cohort labels around the bottom

  # setting default values for missing parameters
  if (is.null(first_age_isoline)) {
    first_age_isoline = first_age
  }
  if (is.null(first_period_isoline)) {
    first_period_isoline = first_period
  }
  if(is.null(isoline_interval)){
    isoline_interval = 2 * interval
  }
  if(is.null(colorbar_range)){ #if color scale is missing use the min and max of data
    colorbar_range[1] <- min(data)
    colorbar_range[2] <- max(data)
  }
  if(is.null(color_map)){
    # define jet colormap
    jet.colors <- colorRampPalette(c("black", "#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
    color_map =  jet.colors(100)
  }
  # end of default values

  m <- dim(data)[1]
  n <- dim(data)[2]

  last_age = first_age + (m - 1) * interval
  last_period = first_period + (n - 1) * interval
  first_cohort = first_period - last_age
  last_cohort = last_period - first_age

  age_isolines = seq(from = first_age_isoline, to = last_age, by = isoline_interval)
  period_isolines = seq(from = first_period_isoline, to = last_period, by = isoline_interval)
  last_age_isoline = tail(age_isolines,1)
  first_cohort_isoline = first_period_isoline - last_age_isoline
  cohort_isolines = seq(from = first_cohort_isoline, to = last_cohort, by = isoline_interval)

  periods <- seq(from = first_period, to = last_period, by = interval)
  ages <- seq(from = first_age, to = last_age, by = interval)
  cohorts <- seq(from = first_cohort, to = last_cohort, by = interval)
  n_ages <- length(ages)
  n_periods <- length(periods)
  n_cohorts <- length(cohorts)

  n_age_isolines <- length(age_isolines)
  n_period_isolines <- length(period_isolines)
  n_cohort_isolines <- length(cohort_isolines)

  # apply the limits to the data by truncating it
  data[data < colorbar_range[1]] = colorbar_range[1]
  data[data > colorbar_range[2]] = colorbar_range[2]


  # === plotting
  ncol <- length(color_map)
  not_nan_data <- !is.nan(data)

  v_data <- as.vector(data[not_nan_data])
  if (colorbar_scale == "Percentile") {
    new_cutpoints <- 0
    n_cutpoints <- 0
    while (n_cutpoints < ncol) { # make sure that the cutoffs are unique and ncol cutoffs are produced
      data_cutpoints <- unique(quantile(x = v_data, probs = seq(from = 0, to = 1, length.out = ncol + new_cutpoints)))
      n_cutpoints <- length(data_cutpoints)
      new_cutpoints <- new_cutpoints + 1
      if (new_cutpoints > ncol * 2) {
        break
      }
    }
  } else { #(colorbar_scale == "Normal"){
    data_cutpoints <- seq(from = colorbar_range[1], to = colorbar_range[2], length.out = ncol)
  }
  datac = cut(data[not_nan_data], #discretize the data
              data_cutpoints,
              include.lowest = T,
              labels = F)
  a <- interval / sqrt(3) # radius of the hexagon (distance from center to a vertex).
  b <- sqrt(3) * a / 2 # half height of the hexagon
  yv <- c(0, b, b, 0, -b, -b, 0)
  xv <- c(-a, -a/2, a/2, a, a/2, -a/2, -a)

  # compute the center of each hexagon by creating an a*p grid for each age-period combination
  P0 <- matrix(periods, nrow = n_ages, ncol = n_periods, byrow = TRUE)
  A0 <- t(matrix(ages, nrow = n_periods, ncol = n_ages, byrow = TRUE))

  # convert the grid to the X-Y coordinate
  X <- compute_xcoordinate(P0)
  Y <- compute_ycoordinate(P0, A0)

  # only keep those that have non-NA values
  X <- X[not_nan_data]
  Y <- Y[not_nan_data]

  # get the color for each level
  color_map2 <- color_map[datac]

  #line vector if hexagon indicate less than 4
  line_map <- ifelse(datac<21, "pink", NA)
  Xvec <- as.vector(X)
  Yvec <- as.vector(Y)
  n_hexagons <- length(Xvec)

  # compute the X and Y cooridinate for each hexagon - each hexagon is a row and each point is a column
  Xhex <- outer(Xvec, xv, '+')
  Yhex <- outer(Yvec, yv, '+')

  minX <- min(Xhex) - interval
  maxX <- max(Xhex) + interval
  maxX2 <- maxX + (maxX - minX) * (colorbar_width * 2)
  if (wrap_cohort_labels) {
    minY <- min(Yhex) - interval
  } else {
    minY <- compute_ycoordinate(p = first_period,
                                a = first_age - (last_period - first_period)) - interval
  }
  maxY <- max(Yhex) + interval

  #  layout(t(1:2),widths=c(4,1)) # two columns - one for the plot, the other for the colorbar

  #par(mar=c(.5,.5,.5,.5))
  #pin <- par("pin")
  plt_wdth <- 4.8 #pin[1]
  plt_ht <- 4.8 #pin[2]

  par(mai = c((1 - zoom_factor)*plt_ht / 2,
              (1 - zoom_factor)*plt_wdth / 2,
              (1 - zoom_factor)*plt_ht / 2,
              (1 - zoom_factor)*plt_wdth / 2))

  plot(x = NULL, y = NULL,
       xlim = c(minX-12,maxX2-3), ylim = c(minY,maxY),
       axes = FALSE, frame.plot = FALSE,
       xaxt = 'n', yaxt = 'n', type = 'n', asp = 1)
  title(main = title_text, line = -1, adj = 0)
  title(main = subtitle_text, line = -2, cex.main = 1, adj = 0)
  title(main = scale_units, line = -3, cex.main = 0.8, adj = 0.5)
  # title(main = prettyNum(subtitle_text, big.mark = ",", trim = TRUE), line = -2, cex.main = 1, adj = 0)
  # title(main = prettyNum(scale_units, big.mark = ",", trim = TRUE), line = -3, cex.main = 1)
  for (i in 1:n_hexagons) {
    polygon(x = Xhex[i, ],   # X-Coordinates of polygon
            y = Yhex[i, ],   # Y-Coordinates of polygon
            col = color_map2[i],  # Color of polygon
            border = NA, # Color of polygon border
            lwd = 1)
  }

  #age-isolines
  y1 <- compute_ycoordinate(first_period,age_isolines)
  y2 <- compute_ycoordinate(last_period + interval,age_isolines)
  x1 <- compute_xcoordinate(first_period)
  x2 <- compute_xcoordinate(last_period + interval)
  for (i in 1:n_age_isolines) {
    lines(x = c(x1,x2), y = c(y1[i], y2[i]), col = line_color, lwd = line_width)
    text(x = x2, y = y2[i], labels = paste("Age:", age_isolines[i]),
         col = label_color, cex = label_size, srt = -30,
         adj = c(0, 0.5))
  }

  # period-isolines
  x <- compute_xcoordinate(period_isolines)
  y1 <- compute_ycoordinate(period_isolines, first_age)
  y2 <- compute_ycoordinate(period_isolines, last_age + interval)

  for (i in 1:n_period_isolines){
    lines(x=c(x[i], x[i]), y=c(y1[i],y2[i]), col = line_color, lwd = line_width)
    text(x=x[i], y=y2[i], labels = paste("Year:",period_isolines[i]),
         col = label_color, cex = label_size, srt = 90,
         adj = c(0, .5))
  }

  # cohort-isolines (need some more processing!)
  # determine the periods where the cohort isolines cross the last age
  p_top <- cohort_isolines + last_age
  p_top <- p_top[p_top < last_period]
  n_top <- length(p_top)
  # and the periods where they cross the first age
  p_bottom <- cohort_isolines + first_age
  p_bottom <- p_bottom[p_bottom > first_period]
  n_bottom <- length(p_bottom)
  # and the ages where they cross the first period
  a_left <- first_period - cohort_isolines
  if (wrap_cohort_labels){
    a_left <- a_left[a_left >= first_age]
  }
  n_left <- length(a_left)
  # and the ages where they cross the last period
  a_right <- last_period - cohort_isolines
  a_right <- a_right[a_right <= last_age]
  n_right <- length(a_right)

  # combine the periods and ages initial and final points on the a*p coordinates
  # first the left-bottom edge
  if (wrap_cohort_labels){
    p1 <- c(rep(first_period, n_left), p_bottom)
    a1 <- c(a_left, rep(first_age, n_bottom))
  } else {
    p1 <- c(rep(first_period, n_left))
    a1 <- c(a_left)
  }
  # then the top-right edge
  p2 <- c(p_top, rep(last_period, n_right))
  a2 <- c(rep(last_age, n_top), a_right)

  # convert the a*p coordinates to x-y coordinates
  x1 <- compute_xcoordinate(p1-interval)
  x2 <- compute_xcoordinate(p2)
  y1 <- compute_ycoordinate(p1-interval, a1-interval)
  y2 <- compute_ycoordinate(p2, a2)
  # finally draw the lines.
  for (i in 1:n_cohort_isolines){
    lines(x=c(x1[i], x2[i]), y=c(y1[i],y2[i]), col = line_color, lwd = line_width)
    text(x=x1[i], y=y1[i], labels = paste("Cohort:",cohort_isolines[i]),
         col = label_color, cex = label_size, srt = 30,
         adj = c(1,.5))
  }
  lines(x=c(x1[9], x2[9]), y=c(y1[9],y2[9]), col = "red", lwd = 1)
  text(x=x1[9], y=y1[9], labels = paste("Cohort:",cohort_isolines[9]),
       col = label_color, cex = label_size, srt = 30,
       adj = c(1,.5))
  # create the colorbar
  # par(las=2)
  # par(mar=c(10,2,10,2.5))
  # #cb_range <- seq(from = colorbar_range[1], to = colorbar_range[2], length.out = ncol)
  # image(y=data_cutpoints,z=t(data_cutpoints), col=color_map, axes=FALSE, main=scale_units, cex.main=.8)
  # axis(4,cex.axis=label_size,mgp=c(0,.5,0))

  # draw the colorbar
  # cb_x1 <- maxX2 - (maxX - minX) * (colorbar_width * 0.5)
  # cb_x2 <- maxX2 - (maxX - minX) * (colorbar_width * 0.25)
  cb_x1 <- maxX2 - (maxX - minX) * (colorbar_width * 0.5) - 11
  cb_x2 <- maxX2 - (maxX - minX) * (colorbar_width * 0.3) - 11
  cb_y2 <- (minY + maxY)/2 + (maxY - minY) * colorbar_height
  cb_y1 <- (minY + maxY)/2 - (maxY - minY) * colorbar_height

  cb_y_points <- seq(from = cb_y1, to = cb_y2, length.out = ncol + 1)
  cb_y_points_1 <- cb_y_points[1:ncol]
  cb_y_points_2 <- cb_y_points[2:ncol+1]
  for (i in 1:ncol){
  polygon(x = c(cb_x1, cb_x2, cb_x2, cb_x1, cb_x1),
          y = c(cb_y_points_1[i], cb_y_points_1[i], cb_y_points_2[i], cb_y_points_2[i], cb_y_points_1[i]),
          col = color_map[i],  # Color of polygon
          border = NA, # Color of polygon border
          lwd = 1)
  }

  # add percentile and label ticks and labels
  n_ptile <- 11
  n_rate_digits <- n_rate_digits
  tick_width <- 0.25 * (cb_x2 - cb_x1)
  ptiles <- seq(0, 100, length.out = n_ptile)
  ptile_pos <- seq(from = cb_y1, to = cb_y2, length.out = n_ptile)
  rate_pos <- c(data_cutpoints[round(ptiles[1:(n_ptile-1)]+1,0)], colorbar_range[2])
  ptile_tick_start_pos <- cb_x1 - tick_width
  rate_tick_start_pos <- cb_x2 + tick_width
  for (i in 1:n_ptile){
    #pctile ticks
    # lines(x = c(ptile_tick_start_pos, cb_x1),
    #       y = c(ptile_pos[i], ptile_pos[i]),
    #       col = label_color, lwd = line_width)
    # text(x = ptile_tick_start_pos - 1, y = ptile_pos[i],
    #      labels = paste(ptiles[i], "%"),
    #      col = label_color, cex = label_size, adj = 1)
    # rate ticks
    lines(x = c(rate_tick_start_pos, cb_x2),
          y = c(ptile_pos[i], ptile_pos[i]),
          col = label_color, lwd = line_width)
    text(x = rate_tick_start_pos + 1, y = ptile_pos[i],
         labels = paste(round(rate_pos[i], n_rate_digits)),
         col = label_color, cex = label_size, adj = 0)
  }

  # add rate labels at equal intervals.
  #n_rates <- 10
  #n_rate_digits <- 0
  #tick_width <- 0.25 * (cb_x2 - cb_x1)
  #rate_ticks <- seq(colorbar_range[1], colorbar_range[2], length.out = n_rates)

}

compute_xcoordinate <- function(p) {
  x <- p * sqrt(3) / 2
  return(x)
}

compute_ycoordinate <- function(p, a){
  y <- a - p / 2
  return(y)
}
