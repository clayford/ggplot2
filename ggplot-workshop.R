# R Graphics with ggplot2
# Fall 2015
# StatLab@UVa Library
# Clay Ford


# Helpful R Studio commands -----------------------------------------------

# Description       Windows & Linux       Mac 
# ----------------------------------------------------------------
# Run current line  Ctrl+Enter            Command+Enter
# Previous plot     Ctrl+Shift+PageUp     Command+Shift+PageUp 
# Next plot 	      Ctrl+Shift+PageDown 	Command+Shift+PageDown 


# Packages ----------------------------------------------------------------

# we'll use the following packages today.
# only submit these lines if you don't already have them installed. 
install.packages("ggplot2")
install.packages("reshape2")
install.packages("scales")
install.packages("RColorBrewer")

library(ggplot2)

# scatterplots ------------------------------------------------------------

# geom_point() 

ggplot(iris, aes(x = Petal.Width, y = Petal.Length)) + 
  geom_point() 

# scatter plot by group
# add color=Species to aes()
ggplot(iris, aes(x = Petal.Width, y = Petal.Length, color=Species)) + 
  geom_point() 

# map Species to shape
ggplot(iris, aes(x = Petal.Width, y = Petal.Length, shape=Species)) + 
  geom_point() 

# shape and color
ggplot(iris, aes(x = Petal.Width, y = Petal.Length, 
                 shape=Species, color=Species)) + 
  geom_point() 

# mapping species to color and shape and making points bigger
ggplot(iris, aes(x = Petal.Width, y = Petal.Length, 
                 color=Species, shape=Species)) + 
  geom_point(size=3) 

# mapping species to color and Sepal.Length to size of dots
# balloon scatter plot
ggplot(iris, aes(x = Petal.Width, y = Petal.Length, 
                 color=Species, size=Sepal.Length)) + 
  geom_point() 

# fix the labels and add a title
ggplot(iris, aes(x = Petal.Width, y = Petal.Length, 
                 color=Species, size=Sepal.Length)) + 
  geom_point() +
  labs(y="Petal Length (cm)", x= "Petal Width (cm)", 
       size="Sepal Length", title="Iris Balloon Plot")

# Your turn!

# The Puromycin data frame has 23 rows and 3 columns of the reaction velocity
# versus substrate concentration in an enzymatic reaction involving untreated
# cells or cells treated with Puromycin.
head(Puromycin)
names(Puromycin)

# Create a scatterplot. Plot conc on the x-axis, rate on the y-axis, and map
# the color of the points to state.
ggplot(Puromycin, aes(x=conc,y=rate, color=state)) + geom_point()


# boxplots ----------------------------------------------------------------

# geom_boxplot()

# Notes: the box is the middle 50% of the data (IQR), the line is the median, 
# the "whiskers" extend to the farthest point that is less than 1.5 x IQR from
# the edge of the box.

ggplot(iris, aes(x=Species, y=Petal.Width)) +
  geom_boxplot()

# thinner boxes
ggplot(iris, aes(x=Species, y=Petal.Width)) +
  geom_boxplot(width=0.5)


# Your Turn!

# PlantGrowth: Results from an Experiment on Plant Growth. A data frame of 30
# cases on 2 variables.
str(PlantGrowth)

# make a boxplot of weight versus group
ggplot(PlantGrowth, aes(x=group, y=weight)) + geom_boxplot()




# strip chart -------------------------------------------------------------

# A one-dimensional scatterplot. These plots are a good alternative to boxplots
# when sample sizes are small.

# An experiment was conducted to measure and compare the effectiveness of
# various feed supplements on the growth rate of chickens.
head(chickwts)
str(chickwts)

# could do a boxplot:
ggplot(chickwts, aes(x=feed, y=weight)) + geom_boxplot()

# Let's try a "stripchart":
ggplot(chickwts, aes(x=feed, y=weight)) + geom_point()

# Notice in the language of ggplot2, we simply call it like it is: geom_point()

# We can actually combine the two:
ggplot(chickwts, aes(x=feed, y=weight)) + geom_boxplot() + geom_point()

# The position argument with the position_jitter() function is useful to prevent
# overplotting. w = 0.1 means side-to-side jitter, h = 0 means no jitter in the
# up-and-down direction.

ggplot(chickwts, aes(x=feed, y=weight)) + 
  geom_point(position = position_jitter(w = 0.1, h = 0))

# Your Turn!

# Make a strip chart of the PlantGrowth data above. That is, plot weight (y)
# versus group (x). Also add some 0.05 side-to-side jitter.
ggplot(PlantGrowth, aes(x=group, y=weight)) + 
  geom_point(position = position_jitter(w = 0.05, h = 0))



# cleveland dot plot ------------------------------------------------------

# Dot plots are a reasonable substitute for bar plots. They use "less ink".

# use state.x77 data; it contains data from 1977 on US States
state.x77[1:6,]

# it's a matrix, so we need to convert to a data frame
class(state.x77)
rownames(state.x77) # row names are the states
states <- data.frame(state=rownames(state.x77),state.x77, row.names = NULL)
head(states)

# population dot plot
ggplot(states, aes(x=Population, y=state)) + geom_point()

# Notice again the language is no different from a scatter plot. We're simply
# plotting points.

# We can use the reorder() function within ggplot to reorder states according to
# population.
ggplot(states, aes(x=Population, y=reorder(state, Population))) +
  geom_point(size=3) +
  labs(title="Population of US States in 1975", y="State") 

# area dot plot
ggplot(states, aes(x=Area, y=reorder(state, Area))) +
  geom_point() +
  labs(title="Land Area of US States", y="State") 

# Notice the x-axis tick marks. Let's fix that.
library(scales) # for comma() function
ggplot(states, aes(x=Area, y=reorder(state, Area))) +
  geom_point() +
  scale_x_continuous(breaks=pretty(range(states$Area)), 
                     labels=comma(pretty(range(states$Area)))) +
  labs(title="Land Area of US States", y="State", x="Area (sq miles)") 

# Your Turn!

# Make a dot plot for Illiteracy by State using the states data frame we
# created. Also reorder the states by Illiteracy and label the y-axis
ggplot(states, aes(x=Illiteracy, y=reorder(state, Illiteracy))) + geom_point() +
  labs(y="State")




# histogram ---------------------------------------------------------------

# Histograms allow us to visualize the distribution of data.

# Let's use the faithful data that comes with R. Waiting time (mins) between 
# eruptions and the duration of the eruption (mins) for the Old Faithful geyser
# in Yellowstone National Park, Wyoming, USA.

head(faithful)

# histogram of eruption times; by default the y axis displays counts in the
# histogram bins.
ggplot(faithful, aes(x=eruptions)) +
  geom_histogram()
# note the warnings; developer firmly believes you should not accept default
# binwidth. use the binwdith argument to change

# with different color bars and banwidth set to 0.1
ggplot(faithful, aes(x=eruptions)) +
  geom_histogram(fill="white", color="black", binwidth=0.1) 

# "true" histogram - density instead of counts
# ..density.. refers to a variable generated by geom_histogram
ggplot(faithful, aes(x=eruptions, y = ..density..)) +
  geom_histogram()

# with a density curve
ggplot(faithful, aes(x=eruptions, y = ..density..)) +
  geom_histogram(binwidth=0.1) +
  geom_density(color="blue")

# back to iris data
# in groups (vertical facet)
ggplot(iris, aes(x=Petal.Length)) +
  geom_histogram(binwidth=0.2) +
  facet_grid(Species ~ .) # vertical ~ horizontal

# in groups (horizontal facet)
ggplot(iris, aes(x=Petal.Length)) +
  geom_histogram(binwidth=0.2) +
  facet_grid(. ~ Species) 

# Your Turn!

# The airquality data set contains air quality measurements in New York, May to
# September 1973.
summary(airquality)

# create a histogram for Temp faceted by Month. Suggested banwidth setting: 2
ggplot(airquality, aes(x=Temp)) + geom_histogram(binwidth=2) +
  facet_grid(.~ Month)


# line graph --------------------------------------------------------------

# Six subjects were given an intravenous injection of indometacin at 11 times,
# and each time plasma concentrations of indometacin was measured.

names(Indometh)
summary(Indometh)

# plot lines for for conc over time for each subject
ggplot(Indometh, aes(x=time,y=conc, group=Subject)) +
  geom_line()

# with color
ggplot(Indometh, aes(x=time,y=conc,group=Subject, color=Subject)) +
  geom_line()

# put legend in numeric order
ggplot(Indometh, aes(x=time,y=conc,group=Subject, color=Subject)) +
  geom_line() +
  scale_color_discrete(limits=1:6)


# Your Turn!

# The nlme package (that comes with R) has a dataset called Oxboys. These data 
# contain the height of 26 boys from Oxford, England recorded over time (age).
# Load the data and plot height (y) versus age (x) with each boy in a single
# plot.

data(Oxboys, package = "nlme")
summary(Oxboys)


ggplot(Oxboys, aes(x=age,y=height, group=Subject)) +
  geom_line()



# bar graph ---------------------------------------------------------------

# bar graphs in ggplot can be a little tricky. By default, it plots a count of 
# items in a category. Technically this is a statistical transformation, 
# stat="bin", which requires your data to have one record per item being
# counted. If your data set already contains counts, you have to specify
# stat="identity".

# Let's do some examples of both scenarios.

# Let's say you recruit 15 people to flip a loaded coin 100 times and record the
# result for each flip. Here's one way to generate the data in R.

# one record per flip per subject
set.seed(1)
result <- sample(c("H","T"), size = 15*100, replace = TRUE, prob = c(0.6,0.4))
subject <- rep(1:15, each = 100)
# recall: data must be in data frame for ggplot
dat <- data.frame(result, subject)
head(dat)
dim(dat)

# make a bar plot of counts for all heads and tails. Notice ggplot2
# automatically tallies up the number of heads and tails. That's because
# geom_bar() uses a default statistical transformation: stat="bin"
ggplot(dat, aes(x=result)) + geom_bar()
ggplot(dat, aes(x=result)) + geom_bar(width=0.5)


# make a bar plot of counts for each subject
ggplot(dat, aes(x=subject, fill=result)) + geom_bar()
# set bars next to each other:
ggplot(dat, aes(x=subject, fill=result)) + geom_bar(position="dodge")

# defaults don't work so great; can fix with scale_x_discrete: set breaks at 1 -
# 15, label each break 1 - 15, and set the limits of the x-axis to go from "1"
# to "15".
ggplot(dat, aes(x=subject, fill=result)) + geom_bar(position="dodge") +
  scale_x_discrete(breaks=1:15, labels=1:15, limits=as.character(1:15))

# using faceting
ggplot(dat, aes(x=result, fill=result)) + geom_bar(width=0.5) + 
  facet_wrap(~ subject)
# no need for legend; remove using + guides(fill=FALSE)
ggplot(dat, aes(x=result, fill=result)) + geom_bar(width=0.5) + 
  facet_wrap(~ subject) + guides(fill=FALSE)

# Now let's say you recruit 15 people to flip a loaded coin 100 times and record the
# result for each subject, that is total heads and tails.

# one record per total count per subject
dat2 <- as.data.frame(table(subject,result))
head(dat2)

# have to use stat="identity" since we already have total counts
ggplot(dat2, aes(x=subject, y=Freq, fill=result)) + 
  geom_bar(stat="identity", position="dodge")

# how to make bar graph of total heads and tails:
# first have to find total and save as data frame
dat3 <- aggregate(Freq ~ result, data=dat2, sum)
dat3
ggplot(dat3, aes(x=result, y=Freq)) + geom_bar(stat="identity", width=0.5)


# Your turn!

# Look at the mtcars data frame. The data was extracted from the 1974 Motor 
# Trend US magazine. Make a bar graph for total count of Transmission type (am =
# 0 or 1). Hint: consider making am a factor.
str(mtcars)

ggplot(mtcars, aes(x=am)) + geom_bar()
ggplot(mtcars, aes(x=factor(am))) + geom_bar(width=0.5)



# Advanced Topics ---------------------------------------------------------

# multiple geoms

# adding fitted regression lines with CI and save
p <- ggplot(iris, aes(x = Petal.Width, y = Petal.Length, color=Species)) + 
  geom_point() +
  geom_smooth(method="lm") + 
  ggtitle("Petals of the Iris data set") 
p  

# "zoom in" on plot with coord_cartesian()
p + coord_cartesian(xlim=c(0,0.75), ylim = c(0, 2))


# add mean to boxplots

# add mean to iris box plots
# notice we can save a ggplot and then call it
bp <- ggplot(iris, aes(x=Species, y=Petal.Width)) + geom_boxplot(width=0.5)
bp
# now add means to the plot using stat_summary():
bp + stat_summary(fun.y = mean, geom="point", color="red", size=3)

# add means and CIs to strip chart

# first need to calculate means and standard errors
fMean <- tapply(chickwts$weight, chickwts$feed, mean)
fSE <- tapply(chickwts$weight, chickwts$feed, function(x) sd(x)/sqrt(length(x)))
# now create data frame; recall that ggplot requires data frame
chick2 <- data.frame(feed=names(fMean), fMean, fSE)

# now plot strip chart and error bars
# notice we call two data frames
sc <- ggplot(chickwts, aes(x=feed, y=weight)) + 
  geom_point(position = position_jitter(w = 0.1, h = 0))
sc

# now add mean and error bars
sc + geom_point(data=chick2, aes(x=feed, y=fMean), color="red", size=3) +
  geom_errorbar(data=chick2, aes(x=feed, y=fMean, 
                                 ymin=fMean - 2*fSE, 
                                 ymax=fMean + 2*fSE), 
                width=0.1)

# another way using stat_summary
ggplot(chickwts, aes(x=feed, y=weight)) + 
  geom_point(position = position_jitter(w = 0.1, h = 0)) +
  stat_summary(fun.data="mean_cl_normal", colour="red", geom="errorbar", width=0.2) +
  stat_summary(fun.y = mean, geom="point", color="red", size=3)


# single line graph of means at each time point with SE bars

# first calculate means and SEs
tMean <- tapply(Indometh$conc, Indometh$time, mean)
tSE <- tapply(Indometh$conc, Indometh$time, function(x)sd(x)/sqrt(length(x)))

# ggplot requires data in data frame
Indo2 <- data.frame(time=unique(Indometh$time), tMean, tSE)

# now ready to create plot
ggplot(Indo2, aes(x=time,y=tMean)) +
  geom_line() +
  geom_point(size=2) +
  geom_errorbar(aes(ymin=tMean-2*tSE, ymax=tMean+2*tSE), width=0.2)


# overlayed histograms with transparency

ggplot(iris, aes(x=Petal.Length, fill=Species)) +
  geom_histogram(position="identity", alpha=0.4, binwidth=.1) 
# position="identity" needed for overlapping; without they get stacked;
# alpha = transparency setting


# saving



# editing guides (legends) 

# plot Ozone vs. Temp with point color mapped to Month; we have to use the 
# factor() function to ensure Month is treated as a categorical variable.
# Compare the difference:

ggplot(airquality, aes(x=Temp, y=Ozone, color=Month)) + geom_point()
ggplot(airquality, aes(x=Temp, y=Ozone, color=factor(Month))) + geom_point()

# The second plot is what we want, but look at the legend title. How can we fix 
# that? A discrete factor (Month) is mapped to color, so we need to change the 
# properties of the color scale. Therefore we use the scale_color_discrete() 
# function. NOTE: month.name is a built-in R function that contains names of
# months in a vector. Below I extract the months numbered 5 - 9.

ggplot(airquality, aes(x=Temp, y=Ozone, color=factor(Month))) + geom_point() +
  scale_color_discrete(name = "Month", labels=month.name[5:9])


# get rid of gray background
ggplot(airquality, aes(x=Temp, y=Ozone, color=factor(Month))) + geom_point() +
  scale_color_discrete(name = "Month", labels=month.name[5:9]) +
  theme_bw()


# help(RColorBrewer) for different palettes 

# others to try: 
# SEQUENTIAL PALETTES (suited to ordered data that progress from low to high): 
# Blues BuGn BuPu GnBu Greens Greys Oranges OrRd PuBu PuBuGn PuRd Purples RdPu
# Reds YlGn YlGnBu YlOrBr YlOrRd

# DIVERGING PALETTES (equal emphasis on mid-range critical values and extremes):
# BrBG PiYG PRGn PuOr RdBu RdGy RdYlBu RdYlGn Spectral

# QUALITATIVE PALETTES (used to create the primary visual differences between classes):
# Accent Dark2 Paired Pastel1 Pastel2 Set1 Set2 Set3

# See the online help for ggplot2; full of examples
# Electronic version of R Graphics Cookbook available from UVa Library

# tidy up before moving on
rm(list=ls())

