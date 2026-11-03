# Hex sticker for hexamap: a hexagonal cluster of hexamap tiles with the
# age, period and cohort isolines crossing at 60 degrees.
#
# Run from the package root with devtools::load_all(). Writes three variants
# to data-raw/sticker/ and copies the chosen one to man/figures/logo.png.

devtools::load_all()

chosen <- "jet-navy"
out_dir <- "data-raw/sticker"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# pointy-top hexagon of radius r centered at (x0, y0), as in the sticker
pointy_hex <- function(r, x0 = 0, y0 = 0) {
  angles <- (90 + 60 * 0:6) * pi / 180
  list(x = x0 + r * cos(angles), y = y0 + r * sin(angles))
}
inside_pointy_hex <- function(x, y, r, x0 = 0, y0 = 0) {
  dx <- abs(x - x0)
  dy <- abs(y - y0)
  dx <= r * sqrt(3) / 2 & dy <= r - dx / sqrt(3)
}

# smooth APC surface: rising with age, a high-risk band of birth cohorts
# and a recent period surge, so all three directions show
rates <- simulate_apc(ages = 0:40, periods = 0:40,
                      age_effect = function(a) 0.04 * a,
                      period_effect = function(p) 0.9 * plogis((p - 25) / 1.5),
                      cohort_effect = function(c) 1.6 * exp(-((c + 6) / 3)^2))

draw_sticker <- function(file, palette, background, border, text_color,
                         isoline_color, family) {
  height_px <- 1200
  width_px <- round(height_px * sqrt(3) / 2)
  grDevices::png(file, width = width_px, height = height_px, res = 300,
                 bg = "transparent", type = "cairo")
  on.exit(grDevices::dev.off())
  graphics::par(mar = rep(0, 4), family = family)
  graphics::plot.new()
  graphics::plot.window(xlim = c(-sqrt(3) / 2, sqrt(3) / 2), ylim = c(-1, 1),
                        asp = 1, xaxs = "i", yaxs = "i")
  graphics::polygon(pointy_hex(1), col = background, border = NA)

  # tiles: center the surface on age 26, period 20 and shrink it so about
  # 14 tiles fit across a cluster of radius 0.52 in the upper part
  lay <- hexamap_layout(rates, first_age = 0, first_period = 0, interval = 1,
                        color_map = palette)
  center <- apc_to_xy(age = 26, period = 20)
  size <- 0.075
  cx <- (lay$cells$x - center$x) * size
  cy <- (lay$cells$y - center$y) * size + 0.25
  keep <- inside_pointy_hex(cx, cy, r = 0.52, y0 = 0.25)
  # spread the palette over the visible tiles only
  lay <- hexamap_layout(rates, first_age = 0, first_period = 0, interval = 1,
                        color_map = palette,
                        colorbar_range = range(lay$cells$value[keep]),
                        colorbar_scale = "log")
  tile_x <- outer(cx[keep], lay$tile$x * size, "+")
  tile_y <- outer(cy[keep], lay$tile$y * size, "+")
  colors <- lay$cells$color[keep]
  graphics::polygon(as.vector(t(cbind(tile_x, NA))), as.vector(t(cbind(tile_y, NA))),
                    col = colors, border = colors, lwd = 0.4)

  # age (-30), period (90) and cohort (30 degree) isolines through the center
  for (angle in c(-30, 90, 30) * pi / 180) {
    len <- 0.45
    graphics::segments(-len * cos(angle), 0.25 - len * sin(angle),
                       len * cos(angle), 0.25 + len * sin(angle),
                       col = isoline_color, lwd = 1.6)
  }

  graphics::text(0, -0.47, "hexamap", col = text_color, cex = 1.9, font = 2)
  graphics::text(0, -0.66, "age · period · cohort", col = text_color,
                 cex = 0.62)
  graphics::polygon(pointy_hex(0.985), col = NA, border = border, lwd = 9)
}

font <- if ("Futura" %in% systemfonts::system_fonts()$family) "Futura" else "sans"
variants <- list(
  "jet-navy" = list(palette = hexamap_palette("jet", 100)[25:100],
                    background = "#0B1D3A", border = "#F5B700",
                    text_color = "white", isoline_color = "white"),
  "viridis-charcoal" = list(palette = hexamap_palette("viridis", 100),
                            background = "#1F1F2E", border = "#FDE725",
                            text_color = "white", isoline_color = "#FFFFFFCC"),
  "jet-white" = list(palette = hexamap_palette("jet", 100)[15:100],
                     background = "white", border = "#0B1D3A",
                     text_color = "#0B1D3A", isoline_color = "white")
)
for (name in names(variants)) {
  v <- variants[[name]]
  draw_sticker(file.path(out_dir, paste0(name, ".png")), v$palette, v$background,
               v$border, v$text_color, v$isoline_color, font)
}

dir.create("man/figures", showWarnings = FALSE, recursive = TRUE)
file.copy(file.path(out_dir, paste0(chosen, ".png")), "man/figures/logo.png",
          overwrite = TRUE)
