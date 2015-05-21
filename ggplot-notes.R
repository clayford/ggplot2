# ggplot2 notes for Fall 2015 workshop

# Basics

# every graph built fromt the same components:
# 1. data
# 2. geometric objects, like dots and lines (called "geoms")
# 3. a coordinate system

# Values in data set mapped to aesthetic properties of the geoms like size,
# color and location (x,y)

# ggplot(data=, aes()) begins a plot that you finish by adding layers.

library(ggplot2)
library(faraway)
ggplot(data = divusa, aes(x=year, y=divorce)) + geom_point()
ggplot(data = divusa, aes(x=year, y=divorce)) + geom_line() + geom_point()
ggplot(data = divusa, aes(x=year, y=birth, size=femlab)) + geom_point()


ggplot(data = divusa, aes(x=year, y=birth)) + geom_line() +
  geom_line(aes(y=divorce), color="red") +
  geom_line(aes(y=marriage), color="blue")

# reshape data to "long" format
library(reshape2)
divusaL <- melt(data = divusa, id.vars = "year", variable.name = "type", value.name = "stat")
divusaL <- subset(divusaL, !(type %in% c("femlab","unemployed")))

ggplot(divusaL, aes(x=year, y=stat, color=type)) + geom_line()

# ggplot() examples

df <- data.frame(gp = factor(rep(letters[1:3], each = 10)),
                 y = rnorm(30))
# Compute sample mean and standard deviation in each group
library(plyr)
ds <- ddply(df, .(gp), summarise, mean = mean(y), sd = sd(y))

# Declare the data frame and common aesthetics.
# The summary data frame ds is used to plot
# larger red points in a second geom_point() layer.
# If the data = argument is not specified, it uses the
# declared data frame from ggplot(); ditto for the aesthetics.
ggplot(df, aes(x = gp, y = y)) +
  geom_point() +
  geom_point(data = ds, aes(y = mean),
             colour = 'red', size = 3)
# Same plot as above, declaring only the data frame in ggplot().
# Note how the x and y aesthetics must now be declared in
# each geom_point() layer.
ggplot(df) +
  geom_point(aes(x = gp, y = y)) +
  geom_point(data = ds, aes(x = gp, y = mean),
             colour = 'red', size = 3)
# Set up a skeleton ggplot object and add layers:
ggplot() +
  geom_point(data = df, aes(x = gp, y = y)) +
  geom_point(data = ds, aes(x = gp, y = mean),
             colour = 'red', size = 3) +
  geom_errorbar(data = ds, aes(x = gp, y = mean,
                               ymin = mean - sd, ymax = mean + sd),
                colour = 'red', width = 0.4)


# Geoms
 
# Stats
 
# Scales
 
# Faceting
 
# Labels
 
# Themes
 
# Legends
 
# Position adjustments
 
# Coordinate Systems, Zooming


# guide_legend() example
library(reshape2) # for melt
df <- melt(outer(1:4, 1:4), varnames = c("X1", "X2"))

p1 <- ggplot(df, aes(X1, X2)) + geom_tile(aes(fill = value))
p1
p2 <- p1 + geom_point(aes(size = value))
p2

# Basic form
p1
p1 + scale_fill_continuous(guide = "legend")
p1 + scale_fill_continuous(guide = guide_legend())

# Guide title

p1 + scale_fill_continuous(guide = guide_legend(title = "V")) # title text
p1 + scale_fill_continuous(name = "V") # same
p1 + scale_fill_continuous(guide = guide_legend(title = NULL)) # no title

# Control styles

# key size
p1 + guides(fill = guide_legend(keywidth = 3, keyheight = 1))

# title position
p1 + guides(fill = guide_legend(title = "LEFT", title.position = "left"))

# title text styles via element_text
p1 + guides(fill = guide_legend(
  title.theme = element_text(size=15, face="italic", colour = "red", angle = 45)))

# label position
p1 + guides(fill = guide_legend(label.position = "bottom"))

# label styles
p1 + scale_fill_continuous(breaks = c(5, 10, 15),
                           labels = paste("long", c(5, 10, 15)),
                           guide = guide_legend(direction = "horizontal", title.position = "top",
                                                label.position="bottom", label.hjust = 0.5, label.vjust = 0.5,
                                                label.theme = element_text(angle = 90)))

# Set aesthetic of legend key

# very low alpha value make it difficult to see legend key
p3 <- qplot(carat, price, data = diamonds, colour = color,
            alpha = I(1/100))
p3

# override.aes overwrites the alpha
p3 + guides(colour = guide_legend(override.aes = list(alpha = 1)))

# multiple row/col legends
p <- qplot(1:20, 1:20, colour = letters[1:20])
p
p + guides(col = guide_legend(nrow = 8))
p + guides(col = guide_legend(ncol = 8))
p + guides(col = guide_legend(nrow = 8, byrow = TRUE))
p + guides(col = guide_legend(ncol = 8, byrow = TRUE))

# reversed order legend
p + guides(col = guide_legend(reverse = TRUE))


# stat_quantile examples
msamp <- movies[sample(nrow(movies), 1000), ]
m <- ggplot(msamp, aes(year, rating)) + geom_point()
m
m + stat_quantile()
m + stat_quantile(quantiles = 0.5)
q10 <- seq(0.05, 0.95, by=0.05)
m + stat_quantile(quantiles = q10)

# You can also use rqss to fit smooth quantiles
m + stat_quantile(method = "rqss")
# Note that rqss doesn't pick a smoothing constant automatically, so
# you'll need to tweak lambda yourself
m + stat_quantile(method = "rqss", lambda = 10)
m + stat_quantile(method = "rqss", lambda = 100)

# Use 'votes' as weights for the quantile calculation
m + stat_quantile(aes(weight=votes))

# Change scale
m + stat_quantile(aes(colour = ..quantile..), quantiles = q10)
m + stat_quantile(aes(colour = ..quantile..), quantiles = q10) +
  scale_colour_gradient2(midpoint = 0.5)

# Set aesthetics to fixed value
m + stat_quantile(colour = "red", size = 2, linetype = 2)

# Use qplot instead
qplot(year, rating, data=movies, geom="quantile")



# Themes

p <- ggplot(mtcars) + geom_point(aes(x = wt, y = mpg,
                                     colour=factor(gear))) + facet_wrap(~am)

p
p + theme_gray()
p + theme_bw()
p + theme_linedraw()
p + theme_light()
p + theme_minimal()
p + theme_classic()


# geom_violin
p <- ggplot(mtcars, aes(factor(cyl), mpg))
p
p + geom_violin()
qplot(factor(cyl), mpg, data = mtcars, geom = "violin")

p + geom_violin() + geom_jitter(height = 0)
p + geom_violin() + coord_flip()
qplot(factor(cyl), mpg, data = mtcars, geom = "violin") +
  coord_flip()

# Scale maximum width proportional to sample size:
p + geom_violin(scale = "count")

# Scale maximum width to 1 for all violins:
p + geom_violin(scale = "width")

# Default is to trim violins to the range of the data. To disable:
p + geom_violin(trim = FALSE)

# Use a smaller bandwidth for closer density fit (default is 1).
p + geom_violin(adjust = .5)

# Add aesthetic mappings
# Note that violins are automatically dodged when any aesthetic is
# a factor
p + geom_violin(aes(fill = cyl))
p + geom_violin(aes(fill = factor(cyl)))
p + geom_violin(aes(fill = factor(vs)))
p + geom_violin(aes(fill = factor(am)))

# Set aesthetics to fixed value
p + geom_violin(fill = "grey80", colour = "#3366FF")
qplot(factor(cyl), mpg, data = mtcars, geom = "violin",
      colour = I("#3366FF"))

# Scales vs. coordinate transforms -------
# Scale transformations occur before the density statistics are computed.
# Coordinate transformations occur afterwards.  Observe the effect on the
# number of outliers.
library(plyr) # to access round_any
m <- ggplot(movies, aes(y = votes, x = rating,
                        group = round_any(rating, 0.5)))
m + geom_violin()
m + geom_violin() + scale_y_log10()
m + geom_violin() + coord_trans(y = "log10")
m + geom_violin() + scale_y_log10() + coord_trans(y = "log10")

# Violin plots with continuous x:
# Use the group aesthetic to group observations in violins
qplot(year, budget, data = movies, geom = "violin")
qplot(year, budget, data = movies, geom = "violin",
      group = round_any(year, 10, floor))


d <- ggplot(diamonds, aes(x = cut, y = clarity))
# By default, all categorical variables in the plot form grouping
# variables, and the default behavior in stat_sum is to show the
# proportion. Specifying stat_sum with no group identifier leads to
# a plot which is not meaningful:
d + stat_sum()
# To correct this problem and achieve a more desirable plot, we need
# to specify which group the proportion is to be calculated over.
# There are several ways to do this:

# by overall proportion
d + stat_sum(aes(group = 1))
d + stat_sum(aes(group = 1)) + scale_size(range = c(3, 10))
d + stat_sum(aes(group = 1)) + scale_size_area(max_size = 10)

# by cut
d + stat_sum(aes(group = cut))
d + stat_sum(aes(group = cut, colour = cut))

# by clarity
d + stat_sum(aes(group = clarity))
d + stat_sum(aes(group = clarity, colour = cut))

# Instead of proportions, can also use sums
d + stat_sum(aes(size = ..n..))

# Can also weight by another variable
d + stat_sum(aes(group = 1, weight = price))
d + stat_sum(aes(group = 1, weight = price, size = ..n..))

# Or using qplot
qplot(cut, clarity, data = diamonds)
qplot(cut, clarity, data = diamonds, stat = "sum", group = 1)



# Generate data
c <- ggplot(mtcars, aes(factor(cyl)))

# By default, uses stat="bin", which gives the count in each category
c + geom_bar()
c + geom_bar(width=.5)
c + geom_bar() + coord_flip()
c + geom_bar(fill="white", colour="darkgreen")

# Use qplot
qplot(factor(cyl), data=mtcars, geom="bar")
qplot(factor(cyl), data=mtcars, geom="bar", fill=factor(cyl))

# When the data contains y values in a column, use stat="identity"
library(plyr)
# Calculate the mean mpg for each level of cyl
mm <- ddply(mtcars, "cyl", summarise, mmpg = mean(mpg))
ggplot(mm, aes(x = factor(cyl), y = mmpg)) + geom_bar(stat = "identity")

# Stacked bar charts
qplot(factor(cyl), data=mtcars, geom="bar", fill=factor(vs))
qplot(factor(cyl), data=mtcars, geom="bar", fill=factor(gear))

# Stacked bar charts are easy in ggplot2, but not effective visually,
# particularly when there are many different things being stacked
ggplot(diamonds, aes(clarity, fill=cut)) + geom_bar()
ggplot(diamonds, aes(color, fill=cut)) + geom_bar() + coord_flip()

# Faceting is a good alternative:
ggplot(diamonds, aes(clarity)) + geom_bar() +
  facet_wrap(~ cut)
# If the x axis is ordered, using a line instead of bars is another
# possibility:
ggplot(diamonds, aes(clarity)) +
  geom_freqpoly(aes(group = cut, colour = cut))

# Dodged bar charts
ggplot(diamonds, aes(clarity, fill=cut)) + geom_bar(position="dodge")
# compare with
ggplot(diamonds, aes(cut, fill=cut)) + geom_bar() +
  facet_grid(. ~ clarity)

# But again, probably better to use frequency polygons instead:
ggplot(diamonds, aes(clarity, colour=cut)) +
  geom_freqpoly(aes(group = cut))

# Often we don't want the height of the bar to represent the
# count of observations, but the sum of some other variable.
# For example, the following plot shows the number of diamonds
# of each colour
qplot(color, data=diamonds, geom="bar")
# If, however, we want to see the total number of carats in each colour
# we need to weight by the carat variable
qplot(color, data=diamonds, geom="bar", weight=carat, ylab="carat")

# A bar chart used to display means
meanprice <- tapply(diamonds$price, diamonds$cut, mean)
cut <- factor(levels(diamonds$cut), levels = levels(diamonds$cut))
qplot(cut, meanprice)
qplot(cut, meanprice, geom="bar", stat="identity")
qplot(cut, meanprice, geom="bar", stat="identity", fill = I("grey50"))

# Another stacked bar chart example
k <- ggplot(mpg, aes(manufacturer, fill=class))
k + geom_bar()
# Use scales to change aesthetics defaults
k + geom_bar() + scale_fill_brewer()
k + geom_bar() + scale_fill_grey()

# To change plot order of class varible
# use factor() to change order of levels
mpg$class <- factor(mpg$class, levels = c("midsize", "minivan",
                                          "suv", "compact", "2seater", "subcompact", "pickup"))
m <- ggplot(mpg, aes(manufacturer, fill=class))
m + geom_bar()



# USING DATA from http://www.bristol.ac.uk/cmm/learning/mmsoftware/data-rev.html
dat <- read.table("data/datasets/chem97.txt", 
                  col.names = c("lea.id","sch.id","ind.id","score","gender","age","avg.score"))

ggplot(dat, aes(x=factor(gender), y=score)) + geom_boxplot()
ggplot(dat, aes(x=age, y=score)) + geom_point()
ggplot(dat, aes(x=age, y=score)) + geom_jitter()
ggplot(dat, aes(x=age, y=score)) + geom_jitter() + facet_grid(~ gender)
ggplot(dat, aes(x=avg.score, y=score)) + geom_point()
ggplot(dat, aes(x=avg.score, y=score)) + geom_jitter()
ggplot(dat, aes(x=avg.score, y=score)) + geom_jitter(alpha=1/10) + facet_grid(~ gender)



library(faraway)
data(jsp)
ggplot(jsp, aes(x=raven, y=math)) + geom_jitter()
ggplot(jsp, aes(x=raven, y=math, color=gender)) + geom_jitter()
ggplot(jsp, aes(x=raven, y=math, color=class)) + geom_jitter()
ggplot(jsp, aes(x=raven, y=math, color=social)) + geom_jitter()
ggplot(jsp, aes(x=raven, y=math, color=gender)) + geom_jitter() + facet_wrap(~ social)
ggplot(jsp, aes(x=raven, y=math, color=gender)) + geom_jitter() + facet_wrap(class ~ social)


ggplot(jsp, aes(x=social, y=math)) + geom_boxplot()
ggplot(jsp, aes(x=social, y=math)) + geom_jitter()
ggplot(jsp, aes(x=social, y=math)) + geom_jitter(position = position_jitter(width = .1))
ggplot(jsp, aes(x=social, y=math, color=gender)) + geom_jitter(position = position_jitter(width = .1))

ggplot(jsp, aes(x=social, y=math, size=raven)) + geom_jitter(position = position_jitter(width = .1))
# how to change scale of raven?
ggplot(jsp, aes(x=social, y=math, size=raven)) + 
  geom_jitter(position = position_jitter(width = .1), alpha=1/8) +
  scale_size_continuous(breaks = seq(5,30,5))


ggplot(jsp, aes(x=gender, y=math)) + geom_boxplot()
ggplot(jsp, aes(x=class, y=math)) + geom_boxplot()

ggplot(jsp, aes(x=raven, y=math)) + geom_jitter() + facet_grid(~ class)
ggplot(jsp, aes(x=raven, y=math)) + geom_jitter() + facet_grid(~ social)

ggplot(jsp, aes(x=raven, y=math)) + geom_jitter() + facet_grid(~ social) +
  geom_smooth()

# ch 9 - manipulating data

library(plyr); library(dplyr) # always load in this order if you need both

# select smallest diamond in each color
ddply(diamonds, .(color), subset, carat == min(carat))

diamonds %>%
  group_by(color) %>%
  filter(carat == min(carat)) %>%
  arrange(color)

# select the two smallest diamonds
ddply(diamonds, .(color), subset, order(carat) <= 2)

diamonds %>%
  group_by(color) %>%
  filter(order(carat) <= 2) %>%
  arrange(color)

# select the 1% largest diamonds in each group
ddply(diamonds, .(color), subset, carat > quantile(carat, 0.99))

diamonds %>%
  group_by(color) %>%
  filter(carat > quantile(carat, 0.99)) %>%
  arrange(color)

# select all diamonds bigger than the group average
ddply(diamonds, .(color), subset, price > mean(price))

diamonds %>%
  group_by(color) %>%
  filter(price > mean(price)) %>%
  arrange(color)

# within each color, scale price to mean 0 and variance 1
head(ddply(diamonds, .(color), transform, price = scale(price)))

diamonds %>%
  group_by(color) %>%
  mutate(price = scale(price)) %>%
  arrange(color)

# subtract off group mean
head(ddply(diamonds, .(color), transform, price = price - mean(price)))

diamonds %>%
  group_by(color) %>%
  mutate(price = price - mean(price)) %>%
  arrange(color)

# apply a function to every column in a data frame
nmissing <- function(x) sum(is.na(x))
nmissing(msleep$name)
nmissing(msleep$brainwt)

nmissing_df <- colwise(nmissing)
nmissing_df(msleep)
# this is shorthand for previous two steps
colwise(nmissing)(msleep)

msleep2 <- msleep[, -6]
numcolwise(median)(msleep2, na.rm = TRUE)
numcolwise(quantile)(msleep2, na.rm = TRUE)
numcolwise(quantile)(msleep2, probs = c(0.25, 0.75), na.rm = TRUE)

# combined with ddply for per-group summaries
ddply(msleep2, .(vore), numcolwise(median), na.rm=T)
ddply(msleep2, .(vore), numcolwise(mean), na.rm=T)

my_summary <- function(df) {
  with(df, data.frame(
    pc_cor = cor(price, carat, method = "spearman"),
    lpc_cor = cor(log(price), log(carat))
    ))
}
ddply(diamonds, .(cut), my_summary)
ddply(diamonds, .(color), my_summary)

diamonds %>%
  group_by(cut) %>%
  summarise(pc_cor = cor(price, carat, method = "spearman"),
            lpc_cor = cor(log(price), log(carat)))

diamonds %>%
  group_by(color) %>%
  summarise(pc_cor = cor(price, carat, method = "spearman"),
            lpc_cor = cor(log(price), log(carat)))

# multiple time series
library(reshape2)
emp  <- melt(economics, id.vars = "date", measure.vars = c("unemploy","uempmed"))
ggplot(emp, aes(x=date, y=value)) + geom_line() + facet_grid(variable ~ ., scales = "free_y")

# fortify
mpgmod <- lm(cty ~ displ, data = mpg)
fortify(mpgmod)

basic <- ggplot(mpgmod, aes(.fitted, .resid)) +
  geom_hline(yintercept = 0, colour = "grey50", size = 0.5) +
  geom_point() +
  geom_smooth(size = 0.5, se=F)
basic
basic + aes(y = .stdresid)
basic + aes(size = .cooksd) + scale_size_area("Cook's distance")

full <- basic %+% fortify(mpgmod, mpg)
full + aes(colour = factor(cyl))
full + aes(displ, colour = factor(cyl))
