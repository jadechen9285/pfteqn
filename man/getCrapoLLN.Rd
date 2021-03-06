% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_Crapo_equations.R
\name{getCrapoLLN}
\alias{getCrapoLLN}
\title{getCrapoLLN}
\usage{
getCrapoLLN(sex, ht, age, value = "FEV1")
}
\arguments{
\item{sex}{string for biological sex (Female vs Male)}

\item{ht}{height in cm}

\item{age}{numeric age (could be in decimal)}

\item{value}{wanted PFT variables}
}
\value{
LLN values for PFT variable input
}
\description{
Crapo Reference Equations for calculating lower limit of norm(LLN)
for PFT variables
}
