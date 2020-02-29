## ------------------------------------------------------------------------
library(trackr)

lb = setRefClass("BasicListBackend", fields = list(dat = "list"))
BasicListBackend = function() lb$new()

## ------------------------------------------------------------------------
setMethod("insert_record", c( target = "BasicListBackend"),
function(object, id, target, opts, verbose = FALSE) {
    target$dat[[id]] = object
    invisible(target)
})

setMethod("remove_record", c(object = "character", target = "BasicListBackend"), 
function(object, target, opts, verbose = FALSE) {
    ## object is the id
    target$dat[[object]] = NULL
    invisible(target)
})

## ------------------------------------------------------------------------
setMethod("trackr_write", c(target = "BasicListBackend"), 
function(target, opts, verbose = FALSE) target)

## ------------------------------------------------------------------------
setMethod("trackr_lookup", c("character", target = "BasicListBackend"),
function(object, target, opts, exist = FALSE){
    found = which(object == names(target$dat))
    if(exist)
        return(length(found) > 0)
    else
        return(backend[[found]])
})

## ------------------------------------------------------------------------

setMethod("trackr_search", c(pattern = "character", target = "BasicListBackend"),
          function(pattern, target, opts, fields = NULL, ret_type = c("id", "list", "backend"),
                   verbose = TRUE) {
    if(is.null(fields)) {
        fields = TRUE ## grab all of them
    }

    inds = sapply(target$dat, function(y)  any(grepl(pattern, paste(y[fields]))))
    names(target$dat)[inds]
    })

## ------------------------------------------------------------------------

library(ggplot2)
plt = qplot(x = 1:10, y = rnorm(10))
be = BasicListBackend()
opts = TrackrOptions(img_dir = tempdir())
db = TrackrDB(backend = be)
defaultTDB(db)
record(plt)
findRecords("rnorm")


## ----generateTags--------------------------------------------------------
y = 5
class(y) = "sillyclass"
setMethod(generateTags, "sillyclass", function(object) "Hi vignette readers!")
fs = makeFeatureSet(y)
tags(fs)

## ----FeatureSet_class----------------------------------------------------
setClass("AwesomeIntFeatureSet", contains = "ObjFeatureSet",
         slots = c(uniquevals = "integer"))

## ----makeFeatureSet------------------------------------------------------
setMethod("makeFeatureSet", "integer", function(object, ...) {
    innerobj = ObjFeatureSet(object, ...) 
    new("AwesomeIntFeatureSet", innerobj, uniquevals = unique(object))
    })

x = sample(1:20, 20, replace=TRUE)
makeFeatureSet(x)@uniquevals

