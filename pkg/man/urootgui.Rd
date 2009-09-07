\name{urootgui}
 \alias{urootgui}
  \title{uroot R-GUI}

  \description{
     Command for launching a graphical user interface for the package \pkg{uroot}.
   }

   \usage{
     urootgui ()
   }

   \details{
     This function requires \code{tcltk}  and \code{BWidget}. The "BWidget" is provided with the package and
     \code{urootgui} adds a path to it.

     All the functions in 'uroot' can be run without launching the GUI.
   }

  \author{Javier López-de-Lacalle \email{javlacalle@yahoo.es} and Ignacio Díaz-Emparanza
    \email{Ignacio.Diaz-Emparanza@ehu.es}.}

   \examples{
     ## Load uroot package.
     # library(uroot)
     ## Launch the GUI.
     # urootgui()
   }

\keyword{misc}


