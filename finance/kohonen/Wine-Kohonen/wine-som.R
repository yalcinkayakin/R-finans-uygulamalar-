
library(kohonen) #loading required package...
data("wines")
WineSc <- scale(wines)
set.seed(7)
Wine.SOM <- som(WineSc, grid=somgrid(5,4,"hexagonal"))
plot(Wine.SOM,main = "Wine data")

colors <- function(n, alpha = 1) {
  rev(heat.colors(n, alpha))
}
plot(Wine.SOM, type = "counts", palette.name = colors, heatkey = FALSE)

plot(Wine.SOM, type = "dist.neighbours", palette.name = terrain.colors)
