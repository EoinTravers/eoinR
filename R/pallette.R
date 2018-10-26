# Some colours
# green = "#4daf4a"
# green2 = "#306E2E"
# orange = "#FFAE00"
# red = "#e41a1c"
# red2 = '#ff5600'
# blue = "#377eb8"

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
