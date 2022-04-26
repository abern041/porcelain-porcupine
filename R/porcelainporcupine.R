porcelainporcupines


porcelain_porcupines <- list(
  GA1 = c("#FFDFFF", "#0080FF", "#FE9E7F", "#03BFC0", "#FE807F", "#C3FFC3", "#05007F", "#E0C1FF"),
  ZABA = c("#462C4C", "#0C4C53", "#CE9B44", "#8E3245", "#283976"),
  HTBAHB = c("#D97839", "#3E8893", "#F7EACB", "#F5C2A9", "#AA4F22", "#C6812D"),
  Dreamland = c("#C8ACCD", "#A6D2CC", "#E06395", "#FE9E7F", "#A0438A", "#FFCCCC"),
  WarmGlow = c("#585234", "#F3E8DF", "#CAAE00", "#7F1C36", "#634836", "#0F1718"),
  Landmark = c("#A0584D", "#91ADC5", "#E3AD58", "#5A5A5C", "#B9A38F", "#3E5D40"),
  Bambi = c("#E7B7AC", "#161613", "#ECA268", "#232D51", "#BC596B", "#007D98"),
  LP3 = c("#F0E1E0", "#604CAC", "#161613", "#D9D9D9"),
  Loser = c("#B7A388", "#8DC1DE", "#BBAFA7", "#080000")
)

porcelain_porcupine <- function(name, n, type = c("discrete", "continuous")) {
  type <- match.arg(type)
  pal <- porcelain_porcupines[[name]]
  if (is.null(pal))
    stop("Palette not found.")

  if (missing(n)) {
    n <- length(pal)
  }

  if (type == "discrete" && n > length(pal)) {
    stop("Number of requested colors greater than what palette can offer")
  }

  out <- switch(type,
                continuous = grDevices::colorRampPalette(pal)(n),
                discrete = pal[1:n]
  )
  structure(out, class = "palette", name = name)
}


print.palette <- function(x, ...) {
  n <- length(x)
  old <- par(mar = c(0.5, 0.5, 0.5, 0.5))
  on.exit(par(old))

  image(1:n, 1, as.matrix(1:n), col = x,
        ylab = "", xaxt = "n", yaxt = "n", bty = "n")

  rect(0, 0.9, n + 1, 1.1, col = rgb(1, 1, 1, 0.8), border = NA)
  text((n + 1) / 2, 1, labels = attr(x, "name"), cex = 1, family = "serif")
}


