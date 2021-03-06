% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Generics.R, R/accessor-methods.R
\docType{methods}
\name{uniqueID<-}
\alias{uniqueID<-}
\alias{uniqueID}
\alias{uniqueID,ANY-method}
\alias{uniqueID,FeatureSet-method}
\alias{uniqueID<-,FeatureSet-method}
\title{Accessor methods for "uniqueid" of FeatureSet objects}
\usage{
uniqueID(object) <- value

uniqueID(object)

\S4method{uniqueID}{FeatureSet}(object)

\S4method{uniqueID}{FeatureSet}(object) <- value
}
\arguments{
\item{object}{An object of (super)class ObjFeatureSet.}

\item{value}{A character vector serving as the unique ID of the plot object in the database.}
}
\value{
A modified object of (super)class ObjFeatureSet.
}
\description{
Get or set the uniqueID field of a FeatureSet. This should not be called directly by end users.
}
