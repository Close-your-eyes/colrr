library(ggplot2)

pak::pak("Close-your-eyes/colrr")
library(colrr)

df <- data.frame(x = rnorm(1000),
                 y = rnorm(1000),
                 z = rnorm(1000))

p <- ggplot(df, aes(x,y,color = z)) +
  geom_point()
p

# color steps by default
p + colrr::get_scale_color_fun(df$z)

# back to normal
p + colrr::get_scale_color_fun(df$z,
                               steps = NULL)

# set number of steps
p + colrr::get_scale_color_fun(df$z,
                               steps = 12)

# set steps exactly
p + colrr::get_scale_color_fun(df$z,
                               steps = c(-2.5,-2,0,1))

# other palette
p + colrr::get_scale_color_fun(df$z,
                               palette = colorRamps::blue2red(n = 50))



# color vecor not z-scored
# colpal_info <- col_pal()
df$z <- runif(1000, max = 9)
p <- ggplot(df, aes(x,y,color = z)) +
  geom_point()

# spectral color palette via col_pal function
p + colrr::get_scale_color_fun(df$z,
                               palette = colrr::col_pal("spectral", direction = -1))

# spectral color palette originally from rcolorbrewer
p + colrr::get_scale_color_fun(df$z,
                               palette = rev(RColorBrewer::brewer.pal(11, "Spectral")))

# rainbow palette
p + colrr::get_scale_color_fun(df$z,
                               palette = grDevices::rainbow(n = 50),
                               steps = 10)

p + colrr::get_scale_color_fun(df$z,
                               palette = colrr::col_pal("spectral", direction = -1),
                               steps = NULL)

p + colrr::get_scale_color_fun(df$z,
                               palette = colrr::col_pal("spectral", direction = -1),
                               steps = 8)

# quantile limits: when cutting the color value range to any percentile, add this info to the color legend
df$z <- scales::squish(df$z, range = c(quantile(df$z, 0.2), quantile(df$z, 0.8)))
max(df$z)
min(df$z)
p <- ggplot(df, aes(x,y,color = z)) +
  geom_point()

p + colrr::get_scale_color_fun(df$z,
                               palette = colrr::col_pal("spectral", direction = -1),
                               qmin = 0.2,
                               qmax = 0.8)

# upper and lower limit missing yet, here
p + colrr::get_scale_color_fun(df$z,
                               palette = colrr::col_pal("spectral", direction = -1),
                               qmin = 0.2,
                               qmax = 0.8,
                               steps = NULL)

p + colrr::get_scale_color_fun(df$z,
                               palette = colrr::col_pal("spectral", direction = -1),
                               qmin = 0.2,
                               qmax = 0.8,
                               steps = 10)





