\name{dat}
\alias{dat}
\docType{data}
\title{
A synthetic data set that was derived from a large scale observational study on youth in substance use treatment.
}
\description{
A subset of measures from the Global Appraisal of Individual Needs biopsychosocial assessment instrument (GAIN) (Dennis, Titus et al. 2003) from sites that administered two different types of substance use disorder treatments (treatment “A” and treatment “B”). The Center for Substance Abuse Treatment (CSAT) funded the sites that administered these two SUD treatments. This dataset consists of 4,000 adolescents, 2,000 in each treatment group. The dataset includes substance use and mental health variables.
}
\usage{data("dat")}
\format{
  A data frame with 4000 observations on the following 29 variables.
  \describe{
    \item{\code{treat}}{a factor with levels \code{A} \code{B}}
    \item{\code{tss_0}}{a numeric vector}
    \item{\code{tss_3}}{a numeric vector}
    \item{\code{tss_6}}{a numeric vector}
    \item{\code{sfs8p_0}}{a numeric vector}
    \item{\code{sfs8p_3}}{a numeric vector}
    \item{\code{sfs8p_6}}{a numeric vector}
    \item{\code{eps7p_0}}{a numeric vector}
    \item{\code{eps7p_3}}{a numeric vector}
    \item{\code{eps7p_6}}{a numeric vector}
    \item{\code{ias5p_0}}{a numeric vector}
    \item{\code{dss9_0}}{a numeric vector}
    \item{\code{mhtrt_0}}{a numeric vector}
    \item{\code{sati_0}}{a numeric vector}
    \item{\code{sp_sm_0}}{a numeric vector}
    \item{\code{sp_sm_3}}{a numeric vector}
    \item{\code{sp_sm_6}}{a numeric vector}
    \item{\code{gvs}}{a numeric vector}
    \item{\code{ers21_0}}{a numeric vector}
    \item{\code{nproc}}{a numeric vector}
    \item{\code{ada_0}}{a numeric vector}
    \item{\code{ada_3}}{a numeric vector}
    \item{\code{ada_6}}{a numeric vector}
    \item{\code{recov_0}}{a numeric vector}
    \item{\code{recov_3}}{a numeric vector}
    \item{\code{recov_6}}{a numeric vector}
    \item{\code{subsgrps_n}}{a numeric vector}
    \item{\code{sncnt}}{a numeric vector}
    \item{\code{engage}}{a numeric vector}
  }
}
\details{
\describe{
    \item{\code{tss_0}}{Traumatic Stress Scale - Baseline}
    \item{\code{tss_3}}{Traumatic Stress Scale - 3 months}
    \item{\code{tss_6}}{Traumatic Stress Scale - 6 months}
    \item{\code{sfs8p_0}}{Substance Frequency Scale - Baseline}
    \item{\code{sfs8p_3}}{Substance Frequency Scale - 3 months}
    \item{\code{sfs8p_6}}{Substance Frequency Scale - 6 months}
    \item{\code{eps7p_0}}{Emotional Problems Scale - Baseline}
    \item{\code{eps7p_3}}{Emotional Problems Scale - 3 months}
    \item{\code{eps7p_6}}{Emotional Problems Scale - 6 months}
    \item{\code{ias5p_0}}{Illegal Activities Scale - baseline}
    \item{\code{dss9_0}}{depressive symptom scale - baseline}
    \item{\code{mhtrt_0}}{mental health treatment in the past 90 days - baseline}
    \item{\code{sati_0}}{substance abuse treatment index - baseline}
    \item{\code{sp_sm_0}}{substance problem scale (past month) - baseline}
    \item{\code{sp_sm_3}}{substance problem scale (past month) - 3 months}
    \item{\code{sp_sm_6}}{substance problem scale (past month) - 6 months}
    \item{\code{gvs}}{General Victimization Scale}
    \item{\code{ers21_0}}{Environmental Risk Scale - baseline}
    \item{\code{ada_0}}{adjusted days abstinent (any in past 90) - baseline}
    \item{\code{ada_3}}{adjusted days abstinent (any in past 90) - 3 months}
    \item{\code{ada_6}}{adjusted days abstinent (any in past 90) - 6 months}
    \item{\code{recov_0}}{in recovery - baseline}
    \item{\code{recov_3}}{in recovery - 3 months}
    \item{\code{recov_6}}{in recovery - 6 months}
    \item{\code{subsgrps_n}}{primarily opioid using youth vs alcohol/marijuana using youth vs                             other}
  }
}
\source{
Diamond, G., Godley, S. H., Liddle, H. A., Sampl, S., Webb, C., Tims, F. M., & Meyers, R. (2002). Five outpatient treatment models for adolescent marijuana use: a description of the Cannabis Youth Treatment Interventions. Addiction, 97, 70-83.
}
\references{
Diamond, G., Godley, S. H., Liddle, H. A., Sampl, S., Webb, C., Tims, F. M., & Meyers, R. (2002). Five outpatient treatment models for adolescent marijuana use: a description of the Cannabis Youth Treatment Interventions. Addiction, 97, 70-83.
}
\examples{
data(dat)
## maybe str(dat) ; plot(dat) ...
}
\keyword{datasets}
