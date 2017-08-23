\name{latest_batting}
\alias{latest_batting}
\title{Get latest batting}
\usage{
latest_batting(yr=2017,npages=3)
}
\description{
This function should download all the innings since 20 August 2017 and reforat them. The number of pages refer to the number of pages of data on ESPN cricinfo. There are 50 innings per page. Setting this too high will throw a lot of error messages, but the data will still be returned. Setting it too low might risk missing some matches. The function is rough and ready, but works OK at the moment. There is always a chance that the interface to cricinfo will change, so not worth making it any more sophisticated.
}
\examples{
Should be self explanatory.
}
