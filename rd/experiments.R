devtools::load_all()

x <- rnorm(1000000, 4.5, 1.2)

pdf <- findpdf(x)

pdf$ranking
pdf$params
