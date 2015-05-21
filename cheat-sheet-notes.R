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

scale_alpha_continuous() # map continuous values to visual values
scale_alpha_discrete() # map discrete values to visual values
scale_alpha_identity() # Use values without scaling.
scale_alpha_manual(values=c()) # Create your own discrete scale

# manual scale examples
p <- qplot(mpg, wt, data = mtcars, colour = factor(cyl))
p
# define your own colors for the legend
p + scale_colour_manual(values = c("red","blue", "green"))
# define your own colors for the legend specifying color to level
p + scale_colour_manual(
  values = c("8" = "red","4" = "blue","6" = "green"))
# With rgb hex values
p + scale_colour_manual(values = c("#FF0000", "#0000FF", "#00FF00"))

# As with other scales you can use breaks to control the appearance
# of the legend
# see ?discrete_scale for more on values, breaks and labels arguments.
cols <- c("8" = "red","4" = "blue","6" = "darkgreen", "10" = "orange")
p + scale_colour_manual(values = cols)
p + scale_colour_manual(values = cols, breaks = c("4", "6", "8"))
p + scale_colour_manual(values = cols, breaks = c("8", "6", "4"))
p + scale_colour_manual(values = cols, breaks = c("4", "6", "8"),
                        labels = c("four", "six", "eight"))


# And limits to control the possible values of the scale
p + scale_colour_manual(values = cols, limits = c("4", "8"))
p + scale_colour_manual(values = cols, limits = c("4", "6", "8", "10"))

# Notice that the values are matched with limits, and not breaks
p + scale_colour_manual(limits = c(6, 8, 4), breaks = c(8, 4, 6),
                        values = c("grey50", "grey80", "black"))

# X AND Y LOCATION SCALES

scale_x_date(labels = date_format(), breaks = date_breaks())
scale_x_datetime()
scale_x_log10()
scale_x_reverse()
scale_x_sqrt()

# scale_x_date examples

# We'll start by creating some nonsense data with dates
df <- data.frame(
  date = seq(Sys.Date(), len=100, by="1 day")[sample(100, 50)],
  price = runif(50)
)
df <- df[order(df$date), ]
dt <- qplot(date, price, data=df, geom="line") + theme(aspect.ratio = 1/4)

# We can control the format of the labels, and the frequency of
# the major and minor tickmarks.  See ?format.Date and ?seq.Date
# for more details.
library(scales) # to access breaks/formatting functions
dt + scale_x_date()
dt + scale_x_date(labels = date_format("%m/%d"))
dt + scale_x_date(labels = date_format("%W"))
dt + scale_x_date(labels = date_format("%W"), breaks = date_breaks("week"))

dt + scale_x_date(breaks = date_breaks("months"),
                  labels = date_format("%b"))
dt + scale_x_date(breaks = date_breaks("4 weeks"),
                  labels = date_format("%d-%b"))

# We can use character string for breaks.
# See \code{\link{by}} argument in \code{\link{seq.Date}}.
dt + scale_x_date(breaks = "2 weeks")
dt + scale_x_date(breaks = "1 month", minor_breaks = "1 week")

# The date scale will attempt to pick sensible defaults for
# major and minor tick marks
qplot(date, price, data=df[1:10,], geom="line")
qplot(date, price, data=df[1:4,], geom="line")

df <- data.frame(
  date = seq(Sys.Date(), len=1000, by="1 day"),
  price = runif(500)
)
qplot(date, price, data=df, geom="line")

# A real example using economic time series data
qplot(date, psavert, data=economics)
qplot(date, psavert, data=economics, geom="path")

end <- max(economics$date)
last_plot() + scale_x_date(limits = c(as.Date("2000-1-1"), end))
last_plot() + scale_x_date(limits = c(as.Date("2005-1-1"), end))
last_plot() + scale_x_date(limits = c(as.Date("2006-1-1"), end))

# If we want to display multiple series, one for each variable
# it's easiest to first change the data from a "wide" to a "long"
# format:
library(reshape2) # for melt
em <- melt(economics, id = "date")

# Then we can group and facet by the new "variable" variable
qplot(date, value, data = em, geom = "line", group = variable)
qplot(date, value, data = em, geom = "line", group = variable) +
  facet_grid(variable ~ ., scale = "free_y")

# COLOR AND FILL SCALES

# Discrete
n + scale_fill_brewer(palette = "Blues")
n + scale_fill_grey(start = 0.2, end = 0.8, na.value = "red")

# Continuous
o <- a + geom_dotplot(aes(fill=..x..))
o + scale_fill_gradient(low = "red", high = "yellow")
o + scale_fill_gradient2(low = "red", high = "blue", mid = "white", midpoint = 25)
o + scale_fill_gradientn(colours = terrain.colors(6))

# SHAPE SCALES

p <- f + geom_point(aes(shape = fl))
p + scale_shape(solid = FALSE)
p + scale_shape_manual(values = c(3:7))

# SIZE SCALES

q <- f + geom_point(aes(size = cyl))
q + scale_size_area(max_size = 6)
q + scale_size_area(max = 6)

# COORDINATE SYSTEMS
r <- b + geom_bar()
r

r + coord_cartesian(xlim = c(0,5))
r + coord_fixed(ratio = 1/100)
r + coord_flip()
r + coord_polar(theta = "x", direction = 1)
r + coord_trans(ytrans = "sqrt")
r + coord_map() # still experimental

# FACETING

# facets divide a plot into subplots based on the values of one or more discrete
# variables.

t <- ggplot(mpg, aes(cty, hwy)) + geom_point()
t

t + facet_grid(. ~ fl)
t + facet_grid(year ~ .)
t + facet_grid(year ~ fl)

t + facet_wrap(~ fl)

# set scales to let axis limits vary across facets
t + facet_grid(year ~ fl, scale="free") 
t + facet_grid(year ~ fl, scale="free_x") 
t + facet_grid(year ~ fl, scale="free_y") 

# set labeller to adjust facet labels
t + facet_grid(.~fl, labeller = label_both)
t + facet_grid(.~fl, labeller = label_bquote(alpha^.(x)))
t + facet_grid(.~fl, labeller = label_parsed)

# better example of label_parsed
mtcars$cyl2 <- factor(mtcars$cyl, labels = c("alpha", "beta", "gamma"))
qplot(wt, mpg, data = mtcars) + facet_grid(. ~ cyl2)
qplot(wt, mpg, data = mtcars) + facet_grid(. ~ cyl2,
                                           labeller = label_parsed)

# LEGENDS
t <- ggplot(mpg, aes(cty, hwy, color=factor(year))) + geom_point()
t

# legend placement
t + theme(legend.position = "bottom")

# remove legend
t + guides(color="none")

# edit legend title and labels
t + scale_color_discrete(name="Year", labels=c("99","08"))
# see ?theme for much more

# THEMES
t + theme_bw()
t + theme_classic()
t + theme_gray()
t + theme_minimal()

# more themes
library(ggthemes)
t + theme_stata()
t + theme_economist()
t + theme_few()

dsamp <- diamonds[sample(nrow(diamonds), 1000), ]
(qplot(carat, price, data=dsamp, colour=clarity)
 + theme_few()
 + scale_colour_few())
(qplot(carat, price, data=dsamp, colour=clarity)
 + theme_few()
 + scale_colour_few("dark"))
(ggplot(diamonds, aes(clarity, fill=cut))
 + geom_bar()
 + theme_few()
 + scale_fill_few("light"))

# ZOOMING
t  <- t + geom_smooth(method="lm")
t

# without clipping (preferred)
t + coord_cartesian(xlim=c(15,20), ylim=c(25,30))

# with clipping (removes unseen data points)
t + xlim(15, 20) + ylim(25, 30)
# or
t + scale_x_continuous(limits=c(15,20)) +
  scale_y_continuous(limits=c(25,30))

# POSITION ADJUSTMENTS

# position adjustments determine how to arrange geoms that would otherwise
# occupy the same space.

s <- ggplot(mpg, aes(fl, fill=drv))

s + geom_bar()
s + geom_bar(position="stack") # default
s + geom_bar(position="dodge")
s + geom_bar(position="fill")

t <- ggplot(mpg, aes(cty, hwy, color=factor(year)))
t + geom_point(position="jitter")
