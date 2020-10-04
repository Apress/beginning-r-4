## ----setup, include=FALSE, results='hide', message = FALSE, warning = FALSE, cache=FALSE----
options(
  width = 70,
  digits = 2)


## -------------------------------------------------------------------
mtcars


## -------------------------------------------------------------------
mtcars$wt
mtcars$mpg


## ----instaR-first-plot, fig.width=6, fig.height=3, out.width='1\\linewidth', fig.pos="!ht", fig.cap = "Plot of mtcars weight and miles per gallon."----
plot(x = mtcars$wt, y = mtcars$mpg)


## ----instaR-first-plotb, fig.width=5, fig.height=5, out.width='.6\\linewidth', fig.pos="!ht", fig.cap = "Plot of mtcars weight and miles per gallon with a different aspect ratio that is roughly equal height and width rather than much wider than tall."----
plot(x = mtcars$wt, y = mtcars$mpg)

