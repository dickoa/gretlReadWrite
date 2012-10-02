\name{write.gdt.matrix}
\alias{write.gdt}
\alias{write.gdt.data.frame}
\alias{write.gdt.matrix}
\title{Export R object data to gretl gdt}
\usage{
  write.gdt.matrix(data, filename, dataname = "Rdata",
    description = "", varlabel = NULL, obslabels = "false",
    encoding = "UTF-8", startobs = 1, endobs = nrow(data),
    frequency = 1, typeofdata = "cross-section")
}
\arguments{
  \item{data}{}

  \item{filename}{}

  \item{dataname}{}

  \item{description}{}

  \item{varlabel}{}

  \item{obslabels}{}

  \item{encoding}{}

  \item{startobs}{}

  \item{endobs}{}

  \item{frequency}{}

  \item{typeofdata}{}
}
\value{
  a gdt file
}
\description{
  Export R object data to gretl gdt
}
\author{
  Ahmadou Dicko <dicko.ahmadou at gmail.com>
}

