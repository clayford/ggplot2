# notes and demo of "data visualization with ggplot2" cheat sheet from RStudio
library(ggplot2)

# Basics ------------------------------------------------------------------

# ggplots is based on the grammar of graphics, the idea you can build every 
# graph from the same few components: data, geoms (visual marks that represent
# data points), and a coordinate system.

# To display data values, map variables in the data set to aesthetic properties
# of the geom like size, color and x and y locations.

# start a plot with ggplot. That begins a plot that you finish by adding layers
# to.

# Add a new layer to a plot with a geom_*() or stat_*() function. Each provides 
# a geom, a set of aesthetic mappings, and a default stat and position
# adjustment.

# Example
ggplot(mtcars, aes(x = wt, y = mpg, color = factor(am))) + geom_point() +
  geom_smooth(method="lm") +
  scale_color_discrete(name="Transmission", labels=c("automatic","manual")) + 
  labs(y="Miles per Gallon", x= "Weight (lb/1000)", title="MPG vs. Weight") +
  theme_bw()

# save last plot as 5' by 5' file named "plot.jpg" in working directory. Matches
# file type to extension.
ggsave("plot.jpg", width = 5, height = 5)


# Geoms -------------------------------------------------------------------

# Use a geom to represet data points, use the geom's aesthetic properties to
# represent variables. Each function returns a layer.


# ONE VARIABLE - continuous

# begin a plot
a <- ggplot(mpg, aes(x=hwy))

# now add layers:
a + geom_area(stat="bin")
a + geom_density()
a + geom_dotplot()

# more on geom_dotplot()
ggplot(mtcars, aes(x = mpg)) + geom_dotplot()
ggplot(mtcars, aes(x = mpg)) + geom_dotplot(binwidth = 1.5)

# y axis isn't really meaningful, so hide it
ggplot(mtcars, aes(x = mpg)) + geom_dotplot(binwidth = 1.5) +
  scale_y_continuous(name = "", breaks = NULL)

a + geom_freqpoly()
a + geom_histogram()

# ONE VARIABLE - discrete

b <- ggplot(mpg, aes(fl))
b + geom_bar()

# TWO VARIABLES - continuous x and y

f <- ggplot(mpg, aes(x=cty, y=hwy))
f + geom_blank()
f + geom_point()
f + geom_jitter()
f + geom_quantile()
f + geom_jitter() + geom_quantile()
f + geom_rug()
f + geom_point() + geom_quantile() + geom_rug()
f + geom_jitter() + geom_quantile() + geom_rug(position = "jitter")

f + geom_jitter() + geom_smooth()

# See stat_smooth for examples of using built in model fitting
# if you need some more flexible, this example shows you how to
# plot the fits from any model of your choosing
qplot(wt, mpg, data=mtcars, colour=factor(cyl))

model <- lm(mpg ~ wt + factor(cyl), data=mtcars)
grid <- with(mtcars, expand.grid(
  wt = seq(min(wt), max(wt), length = 20),
  cyl = levels(factor(cyl))
))
grid$mpg <- predict(model, newdata=grid)
qplot(wt, mpg, data=mtcars, colour=factor(cyl)) + geom_line(data=grid)

# or with standard errors
err <- predict(model, newdata=grid, se = TRUE)
grid$ucl <- err$fit + 1.96 * err$se.fit
grid$lcl <- err$fit - 1.96 * err$se.fit
qplot(wt, mpg, data=mtcars, colour=factor(cyl)) +
  geom_smooth(aes(ymin = lcl, ymax = ucl), data=grid, stat="identity")

f + geom_text(aes(label=cty))

# TWO VARIABLES - discrete x, continuous y
g  <- ggplot(mpg, aes(x=class, y=hwy))
g + geom_bar(stat="identity")
# basically displays the following as a bar graph
aggregate(hwy ~ class, mpg, sum)

g + geom_boxplot()
g + geom_dotplot(binaxis = "y", stackdir = "center")
g + geom_violin()

# TWO VARIABLES - discrete x, discrete y
h  <- ggplot(diamonds, aes(cut, color))
h + geom_jitter()

# CONTINUOUS BIVARIATE DISTRIBUTION
i <- ggplot(movies, aes(year, rating))
i + geom_bin2d()
i + geom_bin2d(binwidth=c(5,0.5))

i + geom_density2d()

# more on geom_density2d()
library("MASS")
data(geyser, "MASS")
m <- ggplot(geyser, aes(x = duration, y = waiting)) +
  geom_point() + xlim(0.5, 6) + ylim(40, 110)
m
m + geom_density2d()

dens <- kde2d(geyser$duration, geyser$waiting, n = 50,
              lims = c(0.5, 6, 40, 110))
densdf <- data.frame(expand.grid(duration = dens$x, waiting = dens$y),
                     z = as.vector(dens$z))
m + geom_contour(aes(z=z), data=densdf)

i + geom_hex()

# CONTINUOUS FUNCTION
j <- ggplot(economics, aes(date, unemploy))
j + geom_area()
j + geom_line()
j + geom_step()

# VISUALIZING ERROR
df <- data.frame(grp=c("A","B"), fit=4:5, se=1:2)
k <- ggplot(df, aes(grp, fit, ymin=fit - se, ymax=fit + se))

k + geom_crossbar()
k + geom_crossbar(fatten=2)

k + geom_errorbar()
k + geom_linerange()
k + geom_pointrange()

# MAPS
dat <- data.frame(murder = USArrests$Murder, state=tolower(rownames(USArrests)))
map <- map_data("state")
l <- ggplot(dat, aes(fill=murder))
l + geom_map(aes(map_id=state), map=map) + expand_limits(x=map$long, y=map$lat)


# THREE VARIABLES

seals$z <- with(seals, sqrt(delta_long^2 + delta_lat^2))
m <- ggplot(seals, aes(long, lat))
m + geom_contour(aes(z = z))

m + geom_raster(aes(fill=z), hjust=0.5, vjust=0.5, interpolate=FALSE)
m + geom_tile(aes(fill=z))


# Stats -------------------------------------------------------------------

# an alternative way to build a layer

# some plots visualize a transformation of the original data set. Use a stat to
# choose a common transformation to visualize.

# Each stat creates additional variables to map aesthetics to. These variables
# use a common ..name.. syntax.

# stat functions and geom functions both combine a stat with a geom to make a
# layer, i.e., stat_bin(geom="bar) does the same as geom_bar(stat="bin")

# 1D distributions
a + stat_bin(binwidth = 1, origin = 10)
a + stat_bin()

a + stat_bindot(binwidth = 1, binaxis = "x")
a + stat_bindot()

a + stat_density(adjust = 1, kernel = "gaussian")
a + stat_density() # same

# 2D distributions

f + stat_bin2d(bins = 30, drop = TRUE)
f + stat_bin2d() # same

f + stat_binhex(bins = 30)
f + stat_binhex() # same

f + stat_density2d(contour = TRUE, n = 100)
f + stat_density2d() # same

# 3 variables

m + stat_contour(aes(z = z))
m + stat_spoke(aes(radius = z, angle = z))
m + stat_summary_hex(aes(z= z), bins = 30, fun = mean)
m + stat_summary2d(aes(z= z), bins = 30, fun = mean)

# Comparisons

g + stat_boxplot(coef = 1.5)
g + stat_boxplot() # same

g + stat_ydensity(adjust = 1, kernel = "gaussian", scale = "area")
g + stat_ydensity() # same

# Functions

f + stat_ecdf(n = 40)
f + stat_ecdf()

f + stat_quantile(quantiles = c(.25, .5, .75), formula = y ~ log(x), method = "rq")
f + stat_quantile()

f + stat_smooth(method = "auto", formula = y ~ x, se = TRUE, n = 80, fullrange = FALSE, level = 0.95)
f + stat_smooth()

# General Purpose 

# draw a N(0,0.5) curve
ggplot() + stat_function(aes(x = -3:3), fun = dnorm, n = 101, args = list(sd = 0.5))
# draw a N(0,1) curve
ggplot() + stat_function(aes(x = -3:3), fun = dnorm, n = 101)

ggplot() + stat_qq(aes(sample = 1:100), distribution = qt, dparams = list(df=5))

# more on stat_q()
# From ?qqplot
y <- rt(200, df = 5)
qplot(sample = y, stat="qq")

# qplot is smart enough to use stat_qq if you use sample
qplot(sample = y)
qplot(sample = precip)

qplot(sample = y, dist = qt, dparams = list(df = 5))

df <- data.frame(y)
ggplot(df, aes(sample = y)) + stat_qq()
ggplot(df, aes(sample = y)) + geom_point(stat = "qq")

# Use fitdistr from MASS to estimate distribution params
library(MASS)
params <- as.list(fitdistr(y, "t")$estimate)
ggplot(df, aes(sample = y)) + stat_qq(dist = qt, dparam = params)

# Using to explore the distribution of a variable
qplot(sample = mpg, data = mtcars)
qplot(sample = mpg, data = mtcars, colour = factor(cyl))

f + stat_sum()
f + stat_summary(fun.data = "mean_cl_boot")
f + stat_unique()


# Scales ------------------------------------------------------------------

# Scales control how a plot maps data values to the visual values of an
# aesthetic. To change the mapping, add a custom scale.

b <- ggplot(mpg, aes(fl))
n <- b + geom_bar(aes(fill=fl))
n
n + scale_fill_manual(values = c("skyblue", "royalblue","blue","navy"),
                      limits = c("d","e","p","r"),
                      breaks = c("d", "e", "p", "r"),
                      name = "fuel",
                      labels = c("D","E","P","R"))
# see discrete_scale for details on arguments


# GENERAL PURPOSE SCALES
# use with any aesthetic: alpha, color, fill, linetype, shape, size

scale_alpha_continuous()

(p <- qplot(mpg, cyl, data = mtcars, alpha = cyl))
ggplot(mtcars, aes(x=mpg, y=cyl, alpha=cyl)) + geom_point()


p + scale_alpha("cylinders")
p + scale_alpha("number\nof\ncylinders")

p + scale_alpha(range = c(0.4, 0.8))

(p <- qplot(mpg, cyl, data=mtcars, alpha = factor(cyl)))
p + scale_alpha_discrete(range = c(0.4, 0.8))

