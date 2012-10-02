\name{read.gdt}
\alias{read.gdt}
\title{Read gretl gdt file}
\usage{
  read.gdt(file, panelindexes = NULL,
    to.data.frame = FALSE)
}
\arguments{
  \item{file}{}

  \item{panelindexes}{}

  \item{to.data.frame}{}
}
\value{
  a data.frame if reading a cross-section data, a mts if
  reading time series object and plm object when reading
  panel data
}
\description{
  This function read gretl gdt file by using the XML
  package to parse the file
}
\examples{
f <- system.file(package = "gretlReadWrite", "data", "classical.gdt")
Data <- read.gdt(f)
str(Data)
}
\author{
  Ahmadou Dicko <dicko.ahmadou at gmail.com>
}

