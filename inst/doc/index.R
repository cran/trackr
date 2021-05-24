## ---- echo = FALSE, message = FALSE-------------------------------------------
knitr::opts_chunk$set(
  error = TRUE,
  tidy = FALSE,
  fig.width = 6, 
  fig.height = 6,
  message = FALSE
  )

## -----------------------------------------------------------------------------
library(trackr)

tdb = TrackrDB(backend = JSONBackend(file = file.path(tempdir(), "objdb.json")),
    img_dir = file.path(tempdir(), "trackr_img_dir"))

defaultTDB(tdb)

## -----------------------------------------------------------------------------

library(ggplot2)
library(lattice)

## examples from 
##      http://www.cookbook-r.com/Graphs
##      https://learnr.files.wordpress.com/2009/08/latbook.pdf
##      http://docs.ggplot2.org/current/index.html
##      http://lmdvr.r-forge.r-project.org/figures/figures.html

## modified version of Figure 1.1 in
## http://lmdvr.r-forge.r-project.org/figures/figures.html
data(Chem97, package = "mlmRev")
pl <- histogram(~gcsescore | factor(score), 
    data = Chem97, main="Lattice Histogram of gcsescore", sub="conditioned on score")
pl
## and a modified version of its ggplot2 counterpart, from
## https://learnr.files.wordpress.com/2009/08/latbook.pdf
pg <- ggplot(Chem97, aes(gcsescore)) + 
    geom_histogram(binwidth = 0.5) + 
    facet_wrap(~score) +
    ggtitle(expression(atop("ggplot2 Histogram of gcsescore", atop("facetted by score")))) +
    theme_bw()
pg

## modified version of a graphic available here:
## http://docs.ggplot2.org/current/scale_hue.html
set.seed(620)
dsamp <- diamonds[sample(nrow(diamonds), 1000), ]
d <- ggplot(dsamp, aes(carat, price, colour = clarity)) +
  ggtitle("Diamond price by carat and clarity") + 
    geom_point() + theme_bw()
d

## modified version of a graphic available here:
## http://docs.ggplot2.org/current/stat_density2d.html
data(geyser, package = "MASS")
m <- ggplot(geyser, aes(x = duration, y = waiting)) +
    geom_point() + geom_density2d() + 
    xlim(0.5, 6) + ylim(40, 110) + 
    xlab("Eruption time in min") + ylab("Waiting time in min") +
    ggtitle("Eruption length and waiting time") +
    coord_trans(y="log10") + theme_bw()
m

## modified version of Figure 2.1 in
## http://lmdvr.r-forge.r-project.org/figures/figures.html
data(Oats, package = "MEMSS")
tp1.oats <- xyplot(yield ~ nitro | Variety + Block, data = Oats, 
    type = 'o',
    xlab = "Nitrogen conc (cwt/acre)",
    ylab = "Yield (bushels/acre)",
    main = "Yield by nitrogen | variety + block")
tp1.oats

## and a modified version of its ggplot2 counterpart, from
## https://learnr.files.wordpress.com/2009/08/latbook.pdf
pg.oats <- ggplot(Oats, aes(nitro, yield)) + 
    geom_line() + geom_point(shape = 'o') +
    facet_grid(Block ~ Variety) +
    xlab("Nitrogen concentration (cwt/acre)") + 
    ylab("Yield (bushels/acre)") + 
    ggtitle("Yeild by nitrogen, facetted by variety and block") + theme_bw()
pg.oats

## Figure 1.2 in
## http://lmdvr.r-forge.r-project.org/figures/figures.html
pl2 <- densityplot(~ gcsescore | factor(score), data = Chem97, 
    plot.points = FALSE, ref = TRUE, main = "Density of gcsescore", subtitle= "conditioned on score")
pl2

## and a modified version of its ggplot2 counterpart, from
## https://learnr.files.wordpress.com/2009/08/latbook.pdf
pg2 <- ggplot(Chem97, aes(gcsescore)) + 
    stat_density(geom = "path", position = "identity") + 
    facet_wrap(~score) + 
    ggtitle(expression(atop("Density of gcsescore", atop(" facetted by score")))) + theme_bw()
pg2

## a modified version of a graphic available here:
## http://docs.ggplot2.org/current/coord_trans.html
df.abc <- data.frame(a = abs(rnorm(26)), letters)
pg3 <- ggplot(df.abc, aes(a, letters)) + 
    geom_point() + coord_trans(x = "sqrt") +
    annotate("text", x = 1.25, y = 5, label = "Some text") + 
    ggtitle("Simulated normal data vs letters") +
    theme_bw()
pg3

## and a lattice version to match
pl3 <- xyplot(letters ~ a, data = df.abc,
   panel = function(x, y,...) {
           panel.xyplot(x, y,...)
           panel.text(1.25, 5, labels = "Some text")
           },
	   main = "Simulated data vs letters v2"
	   )
pl3

## Figure 1.3 in
## http://lmdvr.r-forge.r-project.org/figures/figures.html
pl4 <- densityplot(~ gcsescore, data = Chem97, groups = score,
    plot.points = FALSE, ref = TRUE,
    auto.key = list(columns = 3),
    main = "Densities of gcsescore by score")
pl4

## and a modified version of its ggplot2 counterpart, from
## https://learnr.files.wordpress.com/2009/08/latbook.pdf
pg4 <- ggplot(Chem97, aes(gcsescore)) + 
    stat_density(geom = "path", position = "identity", 
        aes(colour = factor(score))) + 
    ggtitle("Densities of gcsescore by score 2") + theme_bw() +
    theme(legend.position = 'top')
pg4

## a custom example, with both lattice and ggplot2 versions
x = sample(1:5, 1000, replace=TRUE)
y = rnorm(1000, x, sd = 1/sqrt(x))
lp = densityplot(~y, groups = x,
    auto.key=list(space="right", title="x", points=TRUE), main = "More simulated data")
lp

gp = ggplot(data.frame(x = x, y = y), 
    aes(x = y, colour = as.factor(x))) + 
    stat_density(geom = "path", position = "identity") +
    geom_point(aes(y = 0), 
        position = position_jitter(height = 0.01), shape = 21) +
    ggtitle("More simulated data v2") + 
    theme_bw()
gp

## and a couple of base graphics plots
## note that these must be assigned to a variable using recordPlot()
plot(1:10, 1:10, main = "1:10 by 1:10")
pb1 <- recordPlot()

par(mfrow = c(2, 3))
par(cex = 0.6)
par(mar = c(3, 3, 0, 0), oma = c(1, 1, 1, 1))
for (i in 1:6) {
    plot(1, 1, type = "n")
    mtext(letters[i], side = 3, line = -1, adj = 0.1, cex = 0.6)
}
pb2 <- recordPlot()

## -----------------------------------------------------------------------------
record(pl)

## ---- results="hide"----------------------------------------------------------
my.plots <- list(pg = pg,
	 d = d,
	 m = m,
	 tp1.oats = tp1.oats,
	 pg.oats = pg.oats,
	 pl2 = pl2,
	 pg2 = pg2,
	 pg3 = pg3,
	 pl3 = pl3,
	 pl4 = pl4,
	 pg4 = pg4,
	 lp = lp,
	 gp = gp,
	 pb1 = pb1,
	 pb2 = pb2)
invisible(mapply(record, object = my.plots, symorpos = names(my.plots)))

## ---- eval=FALSE--------------------------------------------------------------
#  # Not run
#  knit_and_record("fancyreport.Rmd")

## -----------------------------------------------------------------------------
res = findRecords("density")
res[,"titles"]

## -----------------------------------------------------------------------------
findRecords("[sS]imulated")[,"titles"]

## -----------------------------------------------------------------------------
findRecords("[sS]imulated", fields="titles")[,"titles"]

## -----------------------------------------------------------------------------
findRecords("Oats")[,"titles"]

## -----------------------------------------------------------------------------
diamres = findRecords("diamonds")
diamres[,"titles"]

## -----------------------------------------------------------------------------
rm(d)
print(diamres[[1]]$code)
eval(parse(text = diamres[[1]]$code))
d

## ---- fig.width=12, fig.height = 8--------------------------------------------
gcseplots = findRecords("gcsescore", fields = "code")
library(gridExtra)
plotobjs = lapply(seq(along=gcseplots), function(i) {
	 cd = gcseplots[[i]]$code
	 plt = eval(parse(text = cd))
	 plt
	 })
gallery = marrangeGrob(plotobjs, nrow =2, ncol=3)
gallery

## ---- makemanifest------------------------------------------------------------
library(switchr)
man = manifestFromRecord(diamres[[1]])
man

## ---- eval = FALSE------------------------------------------------------------
#  
#  
#  nm = gsub(":", "_", diamres[[1]]$id)
#  switchTo(nm, seed = man)
#  
#  ## OR
#  
#  install_packages(man)

## ---- eval=FALSE--------------------------------------------------------------
#  
#  ## NOT RUN
#  
#  library(rsolr)
#  
#  blcklght.img.dir <- "~/Downloads/trackr_blacklightapp/app/assets/images"
#  solr.uri <- "http://localhost:8983/solr/figdb"
#  ## non-standard requestHandler not strictly required,
#  ## but will maintain consistency with search results received via UI
#  sl <- SolrList(solr.uri, requestHandler="search")
#  tdb = TrackrDB(TrackrOptions(img_dir = blklght.img.dir), backend = sl)
#  invisible(lapply(my.pfs, record, sl, db = tdb))
#  
#  regDateTime(my.pfs[[9]]) <- Sys.time() - as.difftime(30, unit = "days")
#  ## note that this does not change the unique ID
#  ## (unique ID determined by hash of plot object at creation time of original PlotFeatureSet)
#  ## and so the document is overwritten in the data store.
#  record(my.pfs[[9]], db = tdb, verbose=TRUE)
#  
#  my.pfs[[1]] <- editTags(my.pfs[[1]], c("hello", "world", "goodbye"), option="add")
#  ## same as above (document is overwritten)
#  record(my.pfs[[1]], db = tdb, verbose=TRUE)

## ---- eval=FALSE--------------------------------------------------------------
#  res <- findRecords("density", tdb)
#  ndoc(res)
#  res[1,"image.path"]
#  loadEnrichedPlot(file.path(blcklght.img.dir, res[1,"image.path"]))
#  
#  res <- rmRecord(uniqueID(my.pfs[[1]]),db = tdb)
#  ndoc(res)
#  rmRecord(my.pfs[[1]], db = tdb, verbose=TRUE)
#  res <- findRecords(uniqueID(my.pfs[[1]]), db = tdb, fields="id")
#  ndoc(res)
#  record(my.pfs[[1]], db = tdb, verbose=TRUE)

