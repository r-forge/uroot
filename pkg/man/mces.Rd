\name{mces}
 \docType{data}
  \alias{mces}

  \title{Macroeconomic Time Series from the CAPV}
  \description{
      This data set contains the macroeconomic time series from the Comunidad Autnoma del Pas Vasco [CAPV].
   }

   \format{An .rda file which stores a list with objects of class \code{ts}.}

   \usage{
      data("mces")
   }

   \details{
     Available time series: (Each corresponding label in the list object appears in quotes.)

     \itemize{
       \item "costesal": Wage cost. Sample: 1982.1-2003.2. Quarterly.
       \item "indcsal": Wage cost index. Sample: 1982.1-2003.2. Quarterly.
       \item "ipc": Consumer price index. Sample: 1975.1-2003.10. Monthly.
       \item "ipi": Industrial production index. Sample: 1975.1-2003.9. Monthly.
       \item "ipri": Industrial price index. Sample: 1975.1-2003.10. Monthly.
       \item "mtur": Private car registration. Sample: 1983.1-2003.10. Monthly.
       \item "mvic": Industrial cargo vehicles registration. Sample: 1983.1-2003.10. Monthly.
       \item "paroreg": Registered unemployment. Sample: 1977.1-2003.10. Monthly.
       \item "pernhot": Hotel occupation. Sample: 1987.1-2003.10. Monthly.
       \item "valojhot": Travellers lodged in hotels. Sample: 1982.1-2003.10. Monthly.
       \item "vpoinic": Initiated council houses. Sample: 1972.1-2003.6. Monthly.
       \item "vpoterm": Finished council houses. Sample:  1970.1-2003.6. Monthly.
       \item "epaact": Actives. Sample: 1987.2-2003.3. Quarterly.
     }
   }

   \source{P.H. Franses's homepage: \url{http://www.few.eur.nl/few/people/franses/}.}

   \references{
      P.H. Franses: Periodicity and Stochastic Trends in Economic Time Series (Oxford University Press, 1996).
   }
   \keyword{datagen}


