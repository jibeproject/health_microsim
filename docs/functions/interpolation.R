# functions for interpolation and for plotting interpolated data

# Interpolation function to create smooth data ----

# Spline

library(splines)

disagg_spline <- function(dat, key) {
  epsilon <- 1e-6  # Small constant to avoid log(0)
  
  with(dat, {
    # Define the x and y for interpolation, adding epsilon and log-transforming the rates
    y <- log(rate_1 + epsilon)
    x <- seq(from = floor(min(from_age)/5) * 5, by = 5, length.out = length(y)) + 2
    
    # browser() #useful for checking that x is what we expect. Can be used at any steps that we want 
    # to check the data and works when running the function by stoping the process.
    
    # Generate new x points (high-frequency), constrained to be at most 99
    new_x <- seq(min(x), min(max(to_age), max(x)), length.out = min(100, max(x) - min(x) + 1)) - 2
    
    # browser()
    
    # Perform spline interpolation on the log-transformed data
    log_interpolated <- spline(x, y, xout = new_x)$y
    
    # Transform back from log scale by exponentiating
    interpolated <- exp(log_interpolated) 
    
    # Ensure that negative values do not occur after transformation
    interpolated[interpolated < 0] <- 0
    
    # Create a data frame with the interpolated values
    data.frame(
      ageyr = new_x,
      val_interpolated = interpolated
    )
  })
}


# Polinomial

disagg_polynomial <- function(dat, key) {
  with(dat, {
    y <- rate_1
    x <- seq(from = floor(min(from_age)/5) * 5, by = 5, length.out = length(y)) + 2
    
    # Fit a polynomial model
    fit <- lm(y ~ poly(x, 3))  # 3rd-degree polynomial (adjust degree as needed)
    
    # Generate new x points
    new_x <- seq(min(x), min(max(to_age), max(x)), length.out = min(100, max(x) - min(x) + 1)) - 2
    
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
    x <- seq(from = floor(min(from_age)/5) * 5, by = 5, length.out = length(y)) + 2
    
    # Fit a loess model
    fit <- loess(y ~ x)
    
    # Generate new x points
    new_x <- seq(min(x), min(max(to_age), max(x)), length.out = min(100, max(x) - min(x) + 1)) - 2
    
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

disagg_smooth_spline <- function(dat, key) {
  epsilon <- 1e-6
  
  with(dat, {
    x <- seq(from = floor(min(from_age)/5) * 5, to = max(to_age), by = 5) + 2
    y <- log(rate_1 + epsilon)
    
    # browser()
    
    # Fit a smooth spline model
    fit <- smooth.spline(x, y)
    
    # browser()
    
    # Generate new x points
    new_x <- seq(min(x), min(max(to_age), max(x)), length.out = min(100, max(x) - min(x) + 1)) - 2
    
    # browser()
    
    # Predict values
    log_interpolated <- predict(fit, new_x)$y
    
    # browser()
    
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

# Plot for interpolated data (single location) ----

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



