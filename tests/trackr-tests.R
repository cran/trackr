library(trackr)
library(knitr)
be = JSONBackend(tempfile())
tdb = TrackrDB(backend=be, img_dir = tempdir())
defaultTDB(tdb)

rmdfil = system.file("test_docs/knitr_test.Rmd", package = "trackr")
knit_and_record(rmdfil, verbose = TRUE)
docs = findRecords("mtcars")
stopifnot(length(docs) == 2) ## recvars is 'fit' so that plus report

f = function(paths) readLines(paths)
fil = system.file("test_docs", "knitr_test.Rmd", package = "trackr")
recordFiles(fil, ingestfun = f, verbose = TRUE)

res= findRecords("test_docs")
stopifnot(length(res) == 3, # it doesn't find the "fit" record because not directly associated iwth test_docs
          length(findRecords("*")) == 4)
