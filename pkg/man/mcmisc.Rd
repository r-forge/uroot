\name{mcmisc}
 \docType{data}
  \alias{mcmisc}

  \title{Quarterly Macroeconomic Time Series Data Set}
  \description{
      This data set contains the macroeconomic time series used in the book Franses (1996).
   }

   \format{An .rda file which stores a list with objects of class \code{ts}.}

   \usage{
      data("mcmisc")
   }

   \details{
     Available time series: (Each corresponding label in the list object appears in quotes.)

     \itemize{
       \item "usaipi": Total Industrial Production Index for the United States. Sample: 1960.1 - 1991.4. Source: OECD Main Economic Indicators. Remark: (1985=100).
       \item "canun": Unemployment in Canada. Sample: 1960.1 - 1987.4. Source: OECD Main Economic Indicators. Remark: measured in 1000 persons.
       \item "gergnp": Real GNP in Germany. Sample: 1960.1 - 1990.4. Source: Wolters (1992, p.424, note 4).
       \item "ukinvest": Table DA.3.4: Real Total Investment in the United Kindom. Sample: 1955.1 - 1988.4. Source: Osborn (1990).
       \item "usaipisa": Total Industrial Production Index for the United States. Sample: 1960.1 - 1991.4. Source: OECD Main Economic Indicators. Remark: (1985=100), Seasonally Adjusted.
       \item "canunsa": Unemployment in Canada. Sample: 1960.1 - 1987.4. Source: OECD Main Economic Indicators. Remark: Seasonally Adjusted.
       \item "gergnpsa": Real GNP in Germany. Sample: 1960.1 - 1990.4. Source: Wolters (1992, p.424, note 4). Remark: Seasonally Adjusted.
       \item "ukgdp": United Kingdom gross domestic product. Sample: 1955.1 - 1988.4. Source: Osborn (1990). Remark: at 1985 prices.
       \item "ukcons": United Kingdom total consumption. Sample: 1955.1 - 1988.4. Source: Osborn (1990). Remark: at 1985 prices.
       \item "ukndcons": United Kindom nondurables consumption. Sample: 1955.1 - 1988.4. Source: Osborn (1990). Remark: at 1985 prices.
       \item "ukexp": United Kindom exports of goods and services. Sample: 1955.1 - 1988.4. Source: Osborn (1990). Remark: at 1985 prices.
       \item "ukimp": United Kindom imports of goods and services. Sample: 1955.1 - 1988.4. Source: Osborn (1990). Remark: at 1985 prices.
       \item "ukpinvest": United Kindom public investment. Sample: 1962.1 - 1988.4. Source: Osborn (1990). Remark: at 1985 prices.
       \item "ukwf": United Kindom workforce. Sample: 1955.1 - 1988.4. Source: Osborn (1990). Remark: consisting of workforce in employment and unemployment.
       \item "swndcpc": Real per capita non-durables consumption in Sweden. Sample: 1963.1 - 1988.1. Source: Assarsson (1991). Remark: measured in logs.
       \item "swdipc": Real per capita disposable income in Sweden. Sample: 1963.1 - 1988.1. Source: Assarsson (1991). Remark: measured in logs.
     }
   }

   \source{P.H. Franses's homepage: \url{http://www.few.eur.nl/few/people/franses/}.}

   \references{
      P.H. Franses: Periodicity and Stochastic Trends in Economic Time Series (Oxford University Press, 1996).
   }
   \keyword{datagen}


