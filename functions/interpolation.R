# functions for interpolation and for plotting interpolated data

# Interpolation function to create smooth data ----

# Spline

library(splines)

disagg_spline <- function(dat, key) {
  # 
  # dat <- gbdp_grp %>% ungroup() %>% filter(measure %in% "Incidence") %>% filter(cause %in% "liver_cancer")%>%
  #   filter(location =="Bolton") %>% filter(sex %in% "Female")
  epsilon <- 1e-6  # Small constant to avoid log(0)
  
  with(dat, {
    # Define the x and y for interpolation, adding epsilon and log-transforming the rates
    y <- log(rate_1 + epsilon)
    x <- seq(from = floor(min(from_age)/5) * 5, by = 5, length.out = length(y))
    
    # browser() #useful for checking that x is what we expect. Can be used at any steps that we want 
    # to check the data and works when running the function by stoping the process.
    
    # Generate new x points (high-frequency), constrained to be at most 99
    new_x <- seq(min(x), min(99, max(x)), length.out = min(100, max(x) - min(x) + 1))
    
    # browser()
    
    # Perform spline interpolation on the log-transformed data
    log_interpolated <- spline(x, y, xout = new_x)$y
    
    # browser()
    
    # Transform back from log scale by exponentiating
    interpolated <- exp(log_interpolated) 
    
    # browser()
    
    # Ensure that negative values do not occur after transformation
    interpolated[interpolated < 0] <- 0
    
    # Create a data frame with the interpolated values
    data.frame(
      ageyr = new_x,
      val_interpolated = interpolated
    )
    
    # browser()
  })
}


# Polinomial

disagg_polynomial <- function(dat, key) {
  with(dat, {
    y <- rate_1
    x <- seq(from = floor(min(from_age)/5) * 5, by = 5, length.out = length(y))
    
    # Fit a polynomial model
    fit <- lm(y ~ poly(x, 3))  # 3rd-degree polynomial (adjust degree as needed)
    
    # Generate new x points
    new_x <- seq(min(x), min(99, max(x)), length.out = min(100, max(x) - min(x) + 1))
    
    # Predict values
    interpolated <- predict(fit, newdata = data.frame(x = new_x))
    
    # Ensure that negative values do not occur
    interpolated[interpolated < 0] <- 0
    
    # Create a data frame with the interpolated values
    data.frame(
      ageyr = new_x,
      val_interpolated = interpolated
    )
  })
}


##Loess

disagg_loess <- function(dat, key) {
  with(dat, {
    y <- rate_1
    x <- seq(from = floor(min(from_age)/5) * 5, by = 5, length.out = length(y))
    
    # Fit a loess model
    fit <- loess(y ~ x)
    
    # Generate new x points
    new_x <- seq(min(x), min(99, max(x)), length.out = min(100, max(x) - min(x) + 1))
    
    # Predict values
    interpolated <- predict(fit, newdata = data.frame(x = new_x))
    
    # Ensure that negative values do not occur
    interpolated[interpolated < 0] <- 0
    
    # Create a data frame with the interpolated values
    data.frame(
      ageyr = new_x,
      val_interpolated = interpolated
    )
  })
}


# Smooth spline

# ============================================================
# 2026-08: bands are now anchored at their MIDPOINT, not their start.
#
# Each five-year band summarises the whole interval, so the value for
# [40,45) describes ages 40-44 and is best represented at 42.5, not at
# 40. Anchoring at the band start shifted the entire fitted curve 2.5
# years young, which inflated the within-band means at younger ages
# where the curve is rising steeply..
#
# A smoothing spline is used rather than an interpolating one
# (disagg_spline, above). The two are indistinguishable on band-mean
# accuracy (median absolute deviation 17.2% vs 16.5%), but the
# interpolating spline oscillates between knots: 24 curves showed more
# than five changes of direction against 3 for the smoothing spline,
# with all-cause dementia incidence reversing 10 times in both sexes.
# GBD band rates are themselves modelled estimates rather than raw
# counts, so passing exactly through them buys little.
#
# NOTE: x is built as a regular sequence from the lowest band start to
# 99. This assumes contiguous five-year bands with no gaps. The
# stopifnot below fails loudly if that ever stops holding, rather than
# silently pairing rates with the wrong ages.
# ============================================================

BAND_WIDTH  <- 5
BAND_CENTRE <- BAND_WIDTH / 2   # 2.5: offset from band start to midpoint

disagg_smooth_spline <- function(dat, key) {
  epsilon <- 1e-6
  
  with(dat, {
    band_start <- seq(from = floor(min(from_age) / BAND_WIDTH) * BAND_WIDTH,
                      to = 99, by = BAND_WIDTH)
    x <- band_start + BAND_CENTRE          # midpoint of each band
    y <- log(rate_1 + epsilon)
    
    if (length(x) != length(y)) {
      stop("disagg_smooth_spline: ", length(x), " band positions but ",
           length(y), " rates for ",
           paste(unlist(key), collapse = " / "),
           ". Bands are assumed contiguous and five years wide.")
    }
    
    # Fit a smooth spline model on the log scale
    fit <- smooth.spline(x, y)
    
    # Predict at every single year of age across the banded range.
    # Evaluating outside the fitted knots (below the first midpoint and
    # above the last) extrapolates; smooth.spline does this linearly on
    # the log scale, which is well behaved over 2.5 years at each end.
    new_x <- seq(min(band_start), 99, by = 1)
    
    log_interpolated <- predict(fit, new_x)$y
    
    # Transform back from log scale
    interpolated <- exp(log_interpolated)
    
    # Ensure that negative values do not occur
    interpolated[interpolated < 0] <- 0
    
    # Create a data frame with the interpolated values
    data.frame(
      ageyr = new_x,
      val_interpolated = interpolated
    )
  })
}

# Plot for interpolated data ----

# function to print multiple plots per page
plot_interpolation_pages <- function(plot_data, output_dir) {
  # Define how many plots per page
  nrow <- 3
  ncol <- 2
  
  # Create a combined cause and sex variable for faceting
  plot_data <- plot_data %>%
    mutate(cause_sex = interaction(cause, sex))
  
  # Define the number of unique facets
  n_facets <- length(unique(plot_data$cause_sex))
  
  # Calculate the number of pages needed
  n_pages <- ceiling(n_facets / (nrow * ncol))
  
  # Function to generate a plot for a specific page
  plot_page <- function(page) {
    # Calculate the start and end index for the current page
    start <- (page - 1) * nrow * ncol + 1
    end <- min(page * nrow * ncol, n_facets)
    
    # Subset the data for the current page
    plot_data_subset <- plot_data %>%
      filter(cause_sex %in% unique(plot_data$cause_sex)[start:end])
    
    # Create the plot for the current page
    ggplot(plot_data_subset, aes(x = ageyr, y = value, color = type, linetype = type)) +
      geom_line() +
      facet_wrap(~ cause_sex, nrow = nrow, ncol = ncol, labeller = label_wrap_gen(width = 30)) +
      labs(title = "Original and Interpolated Rates",
           x = "Age",
           y = "Rate",
           color = "Rate Type",
           linetype = "Rate Type") +
      theme_minimal() +
      theme(
        legend.title = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "gray80"),
        panel.grid.minor = element_line(color = "gray90")
      )
  }
  
  for (page in 1:n_pages) {
    p <- plot_page(page)
    file_path <- file.path(output_dir, paste0("plot_page_", page, ".png"))
    ggsave(filename = file_path, plot = p, width = 12, height = 8, bg = "white")
  }
  
}