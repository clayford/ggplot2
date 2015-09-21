# R Graphics with ggplot2
# Fall 2015
# UVa StatLab
# Clay Ford


# Helpful R Studio commands -----------------------------------------------

# Description       Windows & Linux       Mac 
# ----------------------------------------------------------------
# Run current line  Ctrl+Enter            Command+Enter
# Previous plot     Ctrl+Shift+PageUp     Command+Shift+PageUp 
# Next plot 	      Ctrl+Shift+PageDown 	Command+Shift+PageDown 


# Packages ----------------------------------------------------------------

# we'll use the following packages today.
# only submit these lines if you don't already have these packages installed. 
install.packages("ggplot2")
install.packages("reshape2")
install.packages("scales")
install.packages("maps")
install.packages("mapproj")

library(ggplot2)

# scatterplots ------------------------------------------------------------

# Let's start with the tried and true iris data set. This famous data set gives 
# the measurements (in cm) of the variables sepal length and width and petal
# length and width, respectively, for 50 flowers from each of 3 species of iris.

head(iris)
summary(iris)

# scatterplot of petal width and length
ggplot(iris, aes(x = Petal.Width, y = Petal.Length)) + 
  geom_point() 

# scatterplot by group
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

# mapping species to color and shape and making points bigger, plus add fitted
# regression lines:
ggplot(iris, aes(x = Petal.Width, y = Petal.Length, 
                 color=Species, shape=Species)) + 
  geom_point(size=3) +
  geom_smooth(method="lm") 
# have to specify method="lm" for straight regression lines
# lm = linear model; refers to lm() function

# mapping species to color and Sepal.Length to size of dots, aka a bubblechart
ggplot(iris, aes(x = Petal.Width, y = Petal.Length, 
                 color=Species, size=Sepal.Length)) + 
  geom_point() 

# fix the labels and add a title
ggplot(iris, aes(x = Petal.Width, y = Petal.Length, 
                 color=Species, size=Sepal.Length)) + 
  geom_point() +
  labs(y="Petal Length (cm)", x= "Petal Width (cm)", 
       size="Sepal Length", title="Iris Bubblechart")

# Your turn!

# The Puromycin data frame has 23 rows and 3 columns of the reaction velocity
# versus substrate concentration in an enzymatic reaction involving untreated
# cells or cells treated with Puromycin.
head(Puromycin)
summary(Puromycin)

# Create a scatterplot. Plot conc on the x-axis, rate on the y-axis, and map
# the color of the points to state. Try adding geom_smooth().
ggplot(Puromycin, aes(x=conc,y=rate, color=state)) + geom_point()
ggplot(Puromycin, aes(x=conc,y=rate, color=state)) + geom_point() +
  geom_smooth()


# boxplots ----------------------------------------------------------------

# geom_boxplot()

ggplot(iris, aes(x=Species, y=Petal.Width)) +
  geom_boxplot()

# Notes: the box is the middle 50% of the data (IQR), the line is the median, 
# the "whiskers" extend to the farthest point that is less than 1.5 x IQR from
# the edge of the box.

# thinner boxes and bigger, colorful outliers
ggplot(iris, aes(x=Species, y=Petal.Width)) +
  geom_boxplot(width=0.5, outlier.size = 3, outlier.colour = "red")


# Your Turn!

# PlantGrowth: Results from an Experiment on Plant Growth. A data frame of 30
# cases on 2 variables.
summary(PlantGrowth)

# make a boxplot of weight (y-axis) by group (x-axis)
ggplot(PlantGrowth, aes(x=group, y=weight)) + geom_boxplot()


# strip chart -------------------------------------------------------------

# A one-dimensional scatterplot. These plots are a good alternative to boxplots
# when sample sizes are small.

# An experiment was conducted to measure and compare the effectiveness of
# various feed supplements on the growth rate of chickens.
summary(chickwts)
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

# Make a boxplot/stripchart combination of the PlantGrowth data above. That is, 
# plot weight (y) by group (x). Also jitter the points side-to-side with w=0.02.
names(PlantGrowth)
ggplot(PlantGrowth, aes(x=group, y=weight)) + geom_boxplot() +
  geom_point(position = position_jitter(w = 0.02, h = 0))



# cleveland dot plot ------------------------------------------------------

# Dot plots are a reasonable substitute for bar plots. They use "less ink".

# use state.x77 data; it contains data from 1977 on US States
state.x77[1:6,]

# it's a matrix, so we need to convert to a data frame
class(state.x77)
rownames(state.x77) # row names are the states

# Convert matrix to data frame and add states as a column
states <- data.frame(state=rownames(state.x77),state.x77, row.names = NULL)
head(states)
class(states)

# population dot plot
ggplot(states, aes(x=Population, y=state)) + geom_point()

# Notice again the language is no different from a scatter plot. We're simply
# plotting points.

# We can use the reorder() function within ggplot to reorder states according to
# population.
ggplot(states, aes(x=Population, y=reorder(state, Population))) +
  geom_point() +
  labs(title="Population of US States in 1975", y="State") 

# area dot plot
ggplot(states, aes(x=Area, y=reorder(state, Area))) +
  geom_point() +
  labs(title="Land Area of US States", y="State") 

# Notice the x-axis tick marks. Let's fix that.

# Recall that axes and legends are controlled by scales. To modify the x-axis we
# need to use the scale_x_continuous() function. The scales package has some 
# handy functions for formatting numbers, such as comma, dollar and percent.
# Below we use the comma() function to format the labels on the x-axis.

library(scales) # for comma() function
ggplot(states, aes(x=Area, y=reorder(state, Area))) +
  geom_point() +
  scale_x_continuous(labels=comma) +
  labs(title="Land Area of US States", y="State", x="Area (sq miles)") 

# add breaks and labels for every 100,000 sq miles
ggplot(states, aes(x=Area, y=reorder(state, Area))) +
  geom_point() +
  scale_x_continuous(breaks=seq(0,600000,100000), labels=comma) +
  labs(title="Land Area of US States", y="State", x="Area (sq miles)") 

# Your Turn!

# Our states data frame contains a column for Illiteracy, which was the percent 
# of population that was illiterate in 1977. Make a dot plot for Illiteracy by 
# State. Also reorder the states by Illiteracy and format the axis labels as 
# percent (use the percent() function from the scales package.) Hint: divide 
# Illiteracy by 100 so the percents on the x-axis look right when using the
# percent() function.

ggplot(states, aes(x=Illiteracy, y=reorder(state, Illiteracy))) + geom_point() +
  scale_x_continuous(labels=percent)

ggplot(states, aes(x=Illiteracy/100, y=reorder(state, Illiteracy))) + geom_point() +
  scale_x_continuous(labels=percent) 
# or
ggplot(states, aes(x=Illiteracy/100, y=reorder(state, Illiteracy))) + geom_point() +
  scale_x_continuous(breaks=seq(0,0.03,0.005), labels=percent) +
  labs(y="State")



# histogram ---------------------------------------------------------------

# Histograms allow us to visualize the distribution of data.

# Let's use the faithful data that comes with R. Waiting time (mins) between 
# eruptions and the duration of the eruption (mins) for the Old Faithful geyser
# in Yellowstone National Park, Wyoming, USA.

head(faithful)
summary(faithful)
ggplot(faithful, aes(x=eruptions, y=waiting)) + geom_point()

# histogram of eruption times; by default the y axis displays counts in the
# histogram bins.
ggplot(faithful, aes(x=eruptions)) +
  geom_histogram()
# note the warnings; developer firmly believes you should not accept default
# binwidth. use the binwdith argument to change.

# what bandwidth was used? range/30
bw <- diff(range(faithful$eruptions))/30
bw

ggplot(faithful, aes(x=eruptions)) +
  geom_histogram(binwidth=bw) 

# with different color bars and banwidth set to 0.05
ggplot(faithful, aes(x=eruptions)) +
  geom_histogram(fill="white", color="black", binwidth=0.05) 

# "true" histogram - density instead of counts
# ..density.. refers to a variable generated by geom_histogram
ggplot(faithful, aes(x=eruptions, y = ..density..)) +
  geom_histogram(binwidth=0.12)

# with a density curve
ggplot(faithful, aes(x=eruptions, y = ..density..)) +
  geom_histogram(binwidth=0.12) +
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

# use facet_wrap for "pretty" 1d wrapping
ggplot(chickwts, aes(x=weight)) +
  geom_histogram(binwidth=30) +
  facet_wrap( ~ feed) 

# compare to 
ggplot(chickwts, aes(x=weight)) +
  geom_histogram(binwidth=30) +
  facet_grid(. ~ feed) 

# Your Turn!

# The airquality data set contains air quality measurements in New York, May to
# September 1973.
summary(airquality)

# create a histogram for Temp faceted by Month. Suggested banwidth setting: 2. 
# Use facet_wrap( ~ Month)
ggplot(airquality, aes(x=Temp)) + geom_histogram(binwidth=2) +
  facet_wrap(~ Month)


# line graph --------------------------------------------------------------

# geom_line() with "group" aesthetic for lines within groups

# Six subjects were given an intravenous injection of indometacin at 11 times,
# and each time plasma concentrations of indometacin was measured.

names(Indometh)
summary(Indometh)

# plot lines for for conc over time for each subject; notice the "group"
# aesthetic
ggplot(Indometh, aes(x=time,y=conc, group=Subject)) +
  geom_line()

# with color
ggplot(Indometh, aes(x=time, y=conc, group=Subject, color=Subject)) +
  geom_line()

# Notice the legend is not in numeric order. That's because Subject is an
# ordered factor.
class(Indometh$Subject)
levels(Indometh$Subject)

# Remember that scales control legends. So to put the legend in numeric order we
# need to use the scale_color_discrete() function. Simply set the limits to
# range from 1 to 6.
ggplot(Indometh, aes(x=time,y=conc,group=Subject, color=Subject)) +
  geom_line() +
  scale_color_discrete(limits=1:6)

# Let's do a quick example of reshaping data and using line graphs with faceting
head(airquality,3)

# Let's reshape airquality so there's one record per day per measure. The melt()
# function from the reshape2 package makes this pretty easy.

# First make a copy of airquality
aq <- airquality

# Next combine the Day and Month columns into one column called date
aq$date <- as.Date(paste("1973",aq$Month, aq$Day,sep="-"))
# Then drop Month and Day columns since we don't need them
aq$Month <- aq$Day <- NULL 
head(aq, 3)

# now we melt the data, ie make it Long. id.vars = "date" means that's the 
# column that remains and identifies a record. The remaining columns are
# "melted".
aqLong <- melt(aq, id.vars = "date")
head(aqLong)

# now we can make line graphs for each value vs. date faceted by variable
ggplot(aqLong, aes(x=date, y=value)) + geom_line() + 
  facet_wrap( ~ variable)

# Notice all four variables are sharing the same size scale. It's probably 
# better to allow the y-axis to vary. The scales = "free_y" argument allows us
# to do that.
ggplot(aqLong, aes(x=date, y=value)) + geom_line() + 
  facet_wrap( ~ variable, scales = "free_y")


# Your Turn!

# The nlme package (that comes with R) has a dataset called Oxboys. These data 
# contain the height of 26 boys from Oxford, England recorded over time (age). 
# Load the data and plot height (y) versus standardized age (x) for each boy
# (Subject). Don't worry about a legend.
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

# The following data are occurences of cougars in the US. It was downloaded from
# Biodiversity Information Serving Our Nation (BISON),
# http://bison.usgs.ornl.gov.

url <- "http://people.virginia.edu/~jcf2d/workshops/ggplot2/bison-Cougar-20150520-172801.csv"
cougar <- read.csv(url)

# Basis of Record - the type of species occurrence or evidence upon which it is
# based.
summary(cougar$basisOfRecord)

# Let's make a bar graph of that.
ggplot(cougar, aes(x=basisOfRecord)) + geom_bar()

# Easy enough because our data set has one record per occurence.

# What if the data was aggregated, like so:
cougar2 <- as.data.frame(xtabs(~ basisOfRecord, data=cougar))
cougar2

# now try ggplot with geom_bar()
ggplot(cougar2, aes(x=basisOfRecord)) + geom_bar()
# it counted one record each! Not what we wanted.

# We need to change the statistical transformation to stat="identity" and
# specify a y-axis aesthetic.
ggplot(cougar2, aes(x=basisOfRecord, y=Freq)) + geom_bar(stat="identity")

# Notice we could use a dot plot for this graph
ggplot(cougar2, aes(x=Freq, y=basisOfRecord)) + 
  geom_point()

# Perhaps add a line from the point to the y-axis.
ggplot(cougar2, aes(x=Freq, y=basisOfRecord)) + 
  geom_point() +
  geom_segment(aes(xend = 0, yend = basisOfRecord))  +
  labs(y="Basis of Record")



# Your turn!

# Using the cougar data set, create a bar chart of occurences by providedState. 
# Recall each record is an occurence so we just need to make a bar chart of 
# providedState. Hint: add + coord_flip() to rotate your chart and make it look
# nicer.

ggplot(cougar, aes(x=providedState)) + geom_bar() + coord_flip()



# Advanced Topics ---------------------------------------------------------

# saving ggplot graphs
p <- ggplot(iris, aes(x = Petal.Width, y = Petal.Length, color=Species)) + 
  geom_point()

p

# add geoms to saved graph; handy for interactive use; saves typing

p + geom_smooth(method="lm")  
p + geom_smooth(method="lm", se=F)   
p + geom_smooth(method="lm", formula = y ~ poly(x, 3))
p + geom_point(aes(size=Sepal.Width))

# "zoom in" on plot with coord_cartesian()
p + geom_smooth(method="lm", se=F) +
  coord_cartesian(xlim=c(1,2), ylim = c(3, 5))


# add mean to boxplots

# add mean to iris box plots
bp <- ggplot(iris, aes(x=Species, y=Petal.Width)) + geom_boxplot(width=0.5)
bp
# now add means to the plot using stat_summary():
bp + stat_summary(fun.y = mean, geom="point", color="red", size=3)

# add means and CIs to strip chart

# first need to calculate means and standard errors
fMean <- tapply(chickwts$weight, chickwts$feed, mean)
fSE <- tapply(chickwts$weight, chickwts$feed, function(x) sd(x)/sqrt(length(x)))
# now create data frame; recall that ggplot requires data frame
chick2 <- data.frame(feed=names(fMean), fMean, fSE, row.names = NULL)

# now plot strip chart and error bars
# notice we call two data frames
sc <- ggplot(chickwts, aes(x=feed, y=weight)) + 
  geom_point(position = position_jitter(w = 0.1, h = 0))
sc

# now add mean and error bars
sc + geom_point(data=chick2, aes(x=feed, y=fMean), color="#0D3268", size=3) +
  geom_errorbar(data=chick2, aes(x=feed, y=fMean, 
                                 ymin=fMean - 2*fSE, 
                                 ymax=fMean + 2*fSE), 
                width=0.1, color="#F59A2C") +
  labs(title="Mean Weight by Feed Type with 2*SE Bars")

# another way using stat_summary; fun.data="mean_cl_normal" actually calls the 
# function smean.cl.normal() from the Hmisc package. It uses the t distribution
# to determine the multiplier of the standard error.
ggplot(chickwts, aes(x=feed, y=weight)) + 
  geom_point(position = position_jitter(w = 0.1, h = 0)) +
  stat_summary(fun.data="mean_cl_normal", color="#F59A2C", geom="errorbar", width=0.1) +
  stat_summary(fun.y = mean, geom="point", color="#0D3268", size=3)


# single line graph of means at each time point with SE bars

# Recall this plot:
ggplot(Indometh, aes(x=time,y=conc, group=Subject)) +
  geom_line()

# Let's say we wanted to create a single line graph of means at each time point
# with SE bars.

# first calculate means and SEs
tMean <- tapply(Indometh$conc, Indometh$time, mean)
tSE <- tapply(Indometh$conc, Indometh$time, function(x)sd(x)/sqrt(length(x)))

# ggplot requires data in data frame
Indo2 <- data.frame(time=unique(Indometh$time), tMean, tSE, row.names = NULL)

# now ready to create plot
ggplot(Indo2, aes(x=time,y=tMean)) +
  geom_line() +
  geom_errorbar(aes(ymin=tMean-2*tSE, ymax=tMean+2*tSE), width=0.1)

# or again we can use stat_summary()
ggplot(Indometh, aes(x=time,y=conc)) +
  stat_summary(fun.data="mean_cl_normal", geom="errorbar", width=0.1) +
  stat_summary(fun.y = mean, geom="line")

# overlayed histograms with transparency
ggplot(iris, aes(x=Petal.Length, fill=Species)) +
  geom_histogram(position="identity", alpha=0.4, binwidth=.1) 
# position="identity" needed for overlapping; without they get stacked;
# alpha = transparency setting

# maps
library(maps)
library(mapproj) # for the "ployconic" option

states <- map_data("state")
cougar$region <- tolower(cougar$providedState)
cougmap <- merge(states, subset(cougar, !is.na(decimalLongitude)), by = "region")

ggplot(cougmap, aes(long, lat)) +
  borders("state") +
  geom_point(aes(x = decimalLongitude, y=decimalLatitude, color=basisOfRecord)) +
  coord_map("polyconic")

# saving graphs as images

# use ggsave(). It saves the last plot according to your file extension.
ggsave("cougars.jpg", width=10, height=5)



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



# Themes

# Using built-in themes such as theme_bw()
# Before
ggplot(airquality, aes(x=Temp)) + geom_histogram(binwidth=2) +
  facet_wrap(~Month)
# After
ggplot(airquality, aes(x=Temp)) + geom_histogram(binwidth=2) +
  facet_wrap(~Month) + theme_bw()

# To permanently change the theme:
prevTheme <- theme_set(theme_bw())

ggplot(airquality, aes(x=Temp)) + geom_histogram(binwidth=2) +
  facet_wrap(~Month)

ggplot(airquality, aes(x=Temp)) + geom_freqpoly(binwidth=2) +
  facet_wrap(~Month)

# To restore
theme_set(prevTheme)
# verify
ggplot(airquality, aes(x=Temp)) + geom_freqpoly(binwidth=2) +
  facet_wrap(~Month)


# Multiple plots in one window

# In base R, we usually use par(mfrow=c(i,j)), like so:
par(mfrow=c(1,2))
hist(airquality$Temp, main="Distribution of Temp", xlab="Temp")
plot(Ozone ~ Temp, data=airquality, main="Ozone vs. Temp")
par(mfrow=c(1,1))

# We cannot use this approach for ggplot2. The easiest solution is to use the 
# grid.arrange() function in the gridExtra package. To use it, you have to save
# your plots and then call them using grid.arrange().

p1 <- ggplot(airquality, aes(x=Temp)) + geom_histogram(binwidth=4.5) + 
  labs(title="Distribution of Temp")
p2 <- ggplot(airquality, aes(x=Temp, y=Ozone)) + geom_point() +
  labs(title="Ozone vs. Temp")
library(gridExtra)
grid.arrange(p1, p2, nrow=1) # can also use ncol



