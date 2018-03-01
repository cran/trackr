library(trackr)
library(knitr)
be = JSONBackend(tempfile())
tdb = TrackrDB(backend=be, img_dir = tempdir())
defaultTDB(tdb)

rawrmdfil = system.file("test_docs/knitr_test.Rmd", package = "trackr")
rmdfil = file.path(tempdir(), "knitr_test.Rmd")
res = file.copy(from=rawrmdfil, to = rmdfil, overwrite = TRUE)
if(res) {
    knit_and_record(rmdfil)
    docs = findRecords("mtcars")
    stopifnot(ndoc(docs) == 3)
} else {
    warning("file copy from ", rawrmdfil, " to ", rmdfil, " failed or pandoc not found. unable to perform knit_and_Record test")
}
