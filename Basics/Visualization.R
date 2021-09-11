
###################################
#
# THIS SCRIPT INCLUDES USEFUL FUNCTIONS
# FOR DATA VISUALIZATION WITH GGPLOT2
#
####################################




# GGPLOT ------------------------------------------------------------------

# ggplot() is the basic function required for all plot with ggplot.
# The first argument indicates the dataset to be used.
# The second argument indicates aesthetics, i.e. content that depends on the data. At minimum, this requires an x variable. For some plots, a y variabel is required as well.

ggplot(data, aes(x = Variable1, y = Variable2))


# AESTHETICS & ATTRIBUTES -------------------------------------------------


# Can be used to further change the appearance of a plot in combination with geoms.
# The same function can be used both as an aestethic or an attribute.
# If placed within aes(), it is called an aesthetic. This reveals further information on the data, as it refers to a variable.

ggplot(data, aes(Variable1, Variable2, color = Variable3, size = Variable4)) +
  geom_xy()



# If placed outside aes(), it is called an attribute. This has no influence on what the data tells, it only refers to general indicators.

ggplot(data, aes(Variable1, Variable2), color = "blue", size = 4) +
  geom_xy()



# Any attribute or aesthetic, including x and y variables, can also be placed inside a single geom instead of ggplot().
# It is then only applied to this very geom rather than the whole plot.

ggplot(data, aes(Variable1, Variable2)) +
  geom_xy(aes(color = Variable3), size = 4)



# Changes the color.
# Sometimes, this only refers to the outline of a geom.

color


# Changes the color filling.
# If color does only change the outline, this can be used to fill the inside.

fill


# Changes the size.

size


# Changes the shape.

shape


# Changes the transparency.
# If used as an attribute, a number from 0-1 should be used.

alpha


# Adds labels.
# Works only well when combined with geom_text().

label


# Changes how the plot is being arranged.
# Works well with geom_bar or geom_histogram. Only used as attribute.

position =
  stack OR position_stack()
  jitter OR position_jitter(alpha = 0.3)
  dodge OR position_dodge(width = 0.5, preserve = "single")
  fill OR position_fill()
  identity OR position_identity()


# Treats the variable as factor and arranges it in descending order.
# Adding fct_rev in front arranges it in ascending order.

ggplot(data, aes(x = fct_infreq(Variable)) +
  geom_xy()


ggplot(data, aes(x = fct_rev(fct_infreq(Variable))) +
  geom_xy()

# GEOMS -------------------------------------------------------------------


# Creates a scatterplot.
# Requires two numerical variables.

ggplot(data, aes(x = Variable1, y = Variable2)) +

  geom_point()

# Creates a scatterplot, but randomly moves the points.
# Requires two numerical variables.
# The arguments within the geom are given in the units if the axes.

ggplot(data, aes(x = Variable1, y = Variable2)) +

  geom_jitter(height = 100, width = 50)


# Creates a trendline.
# Requires two numarical variables.

ggplot(data, aes(x = Variable1, y = Variable2)) +

  geom_smooth(method = "lm"/"loess")


# Creates a line plot.
# Requires two numerical variables.

ggplot(data, aes(x = Variable1, y = Variable2)) +

  geom_line()


# Creates a bar plot, with only the count of observations on the y axis.
# Requires one categorical variable.

ggplot(data, aes(x = Variable1, y = Variable2)) +

  geom_bar()


# Creates a bar plot, but with specified values on the y axis.
# Requires one or several variables, one should be categorical.

ggplot(data, aes(x = Variable1, y = Variable2)) +

  geom_col()


# Creates a histogram.
# Requires one numerical variable. The second one can be used to indicate a relative scale.

ggplot(data, aes(x = Variable1, y = ..density..)) +

  geom_histogram(bins = 2, binwidth = 5)


# Creates a boxplot
# Requires two variables. x must be categorical, y numerical.

ggplot(data, aes(x = Variable1, y = Variable2)) +

  geom_boxplot()


# THEMES ------------------------------------------------------------------

# Changes the position of the legend.

theme(

  legend.position = "top"/"bottom"/"right"/"left"/"none"

  legend.position = c(0,0) [bottom-left] / c(1,1) [top-right]
)


# Changes the appearance of a any line, text, or rectangular object in the plot.
# Use element_blank() to remove an element

theme(

  axis.line = element_line(color = "blue", linetype = "dashed")

  text = element_text(family = "Bookman")

  rect = element_rect(fill = "white")

  xyz = element_blank()

)


# Modifies whitespaces and non-visible margins.

theme(

  axis.ticks.length = unit(3, "lines"/"pt"/"cm")

  legend.margin = margin(top, right, bottom, left, "lines"/"pt"/"cm")

)

#

# ADDITIONAL LAYERS -------------------------------------------------------


# Splits a plot into different areas, depending on an indicated variable.
# Works well with geom_point().

ggplot(data, aes(x = Variable1, Variable2)) +
  geom_point() +

  facet_grid(. ~ VariableXaxis)

  facet_grid(VariableYaxis ~ .)



# Works like facet_grid, but only the x variable can be indicated.
# nrow or ncol indicates the number of rows or columns the splitted plot should have.


ggplot(data, aes(x = Variable1, Variable2)) +
  geom_point() +

    facet_wrap(~ VariableXaxis, nrow = 4, nco, = 5)



# Adds labels.

ggplot(data, aes(x = Variable1, Variable2)) +
  geom_point() +

  labs(x = "x axis label", y = "y axis label", title = "title", caption = "caption")


# Defines properties of the color scale, e.g. when having a legend.
# The first argument indicates the title, the second is a vector with colors to use.

ggplot(data, aes(x = Variable1, Variable2)) +
  geom_point() +

  scale_color_manual("title", values = palette)


# Can be used to determine the scale limits of the axes.

ggplot(data, aes(x = Variable1, Variable2)) +
  geom_point() +

  xlim(2,-2)
  ylim(2,-2)



