\name{latest_bowling}
\alias{latest_bowling}
\title{Get latest bowling}
\usage{
latest_bowling(yr=2017,npages=3)
}
\description{
This function should download all the innings since 5 August 2017 and reformat them. The number of pages refer to the number of pages of data on ESPN cricinfo. There are 50 innings per page. Setting this too high will throw a lot of error messages, but the data will still be returned. Setting it too low might risk missing some matches. The function is rough and ready, but works OK at the moment. There is always a chance that the interface to cricinfo will change, so not worth making it any more sophisticated.
}
\examples{
d<-latest_bowling()
## Ignore the try errors. Just check that it's worked.
str(d)
updated_bowling<-dplyr::bind_rows(d,bowling)
## Should be OK, but may be some coercions.
}
