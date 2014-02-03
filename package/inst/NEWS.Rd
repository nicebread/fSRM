% \newcommand{\PR}{\Sexpr[results=rd]{tools:::Rd_expr_PR(#1)}}

\name{fSRM}
\title{News for package 'fSRM'}
\encoding{UTF-8}


\section{Version 0.3.1}{

\subsection{Changes}{
	\itemize{
		\item Pretty inst/News.Rd file
		\item Minor changes for passing R CMD check ...
	}
}
}


\section{Version 0.3}{

\subsection{New Features}{
	\itemize{
		\item renamed data sets and changed some column names from the output
		\item Now works with missing values. Default behavior: Families with missing values are excluded (listwise deletion). Set \code{fSRM(..., missing="fiml")} for ML / FIML estimation
		\item More consistent behavior of parameters \code{means}, \code{diff}, and \code{group} in fSRM. It does now that what would be expected.
	}
}
}


\section{Version 0.2.6}{

\subsection{New Features}{
	\itemize{
		\item mod() function now works with mean structure and delta method if lavaan >= 0.5.16 is installed
		\item added a data set based on Eichelsheim (2011): \code{data(two.groups)}
		\item Fixed minor bug in print.fSRM with multiple groups
		\item Now prints a warning if it is attempted to automatically set negative variances to zero in the multiple-group case (Not implemented yet).
		\item Added helper functions which help to import data sets from different formats: `import()`
		\item Negative variances can be automatically set to zero: \code{setZero = "negative"}, or set all non-significant variances to zero: setZero = \code{"nonsig"}. Please note: The purpose of this function is to reproduce published results; usually it is *not* recommended to set non-significant variances to zero! This parameter does not work yet with multiple groups.
		\item Included two data sets, based on Bill Cook's (2000) study: Attachment anxiety in 4-person families (three.person and four.person, see also http://davidakenny.net/kkc/c9/c9.htm), and attachment dependency (two.indicators)
		\item renamed parameter for `drop`: before: \code{"reciprocities"}, now: \code{"GR"}
		\item renamed parameter for fSRM: before: \code{"delta"}, now: \code{"diff"}
		\item minor fixes
	}
}
}




\section{Version 0.2.2}{

\subsection{New Features}{
	\itemize{
		\item Delta method now also compares variances between two groups
		\item Delta method works for 3-person families, and with dropping of factors
		\item equalMeans() now provides a Wald test for equality of relationship effect means
	}
}



\subsection{New Features}{
	\itemize{
		\item \code{drop} does not remove the factor, but rather sets its variance to zero.
		\item Improved output of the mod() function
		\item Mean structure for 3-person family now works correctly
		\item The family effect now is included in the delta method
		}
}
}


\section{Version 0.2.1}{

\subsection{New Features}{
	\itemize{
		\item Replaced `fe` parameter in fSRM by `drop`: Now the user can drop either the family effect, the actor, the partner effects, or the reciprocities when only three members are present.
		\item New function equalMeans() tests actor and partner means for equality
		\item Implemented delta method: all means of two groups can be compared simultaneously. Set \code{delta=TRUE} and \code{group='group'} in fSRM, and you're done!
	}
}

\subsection{Changes}{
	\itemize{
		\item The structure of the raw data is not shown anymore when executing the fSRM function
		\item Fixed some inconsistencies in labeling
		\item Preliminarily dropped analyses with self-ratings (they probably will be added later again)
		\item Removed error correlations type `within': This has been described in the literature, but actually never been used.
	}
}
}