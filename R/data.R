#' Longitudinal observational data from adolescents receiving SUD treatment.
#'
#' A dataset containing substance use disorder and mental health measures for
#' adolescents who had one of two substance use treatments
#'
#' @format A tibble with columns:
#' \describe{
#'  \item{treat}{Substance Use Treatment Indicator, Treatment A and Treatment B}
#'  \item{tss_0}{Traumatic Stress Scale, baseline}
#'  \item{tss_3}{Traumatic Stress Scale, recorded at 3-months}
#'  \item{tss_6}{Traumatic Stress Scale, recorded at 6-months}
#'  \item{sfs8p_0}{Substance Frequency Scale, baseline}
#'  \item{sfs8p_3}{Substance Frequency Scale, recorded at 3-months}
#'  \item{sfs8p_6}{Substance Frequency Scale, recorded at 6-months}
#'  \item{eps7p_0}{Emotional Problems Scale, baseline}
#'  \item{eps7p_3}{Emotional Problems Scale, recorded at 3-months}
#'  \item{eps7p_6}{Emotional Problems Scale, recorded at 6-months}
#'  \item{ias5p_0}{Illegal Activity Scale (baseline)}
#'  \item{dss9_0}{Depressive Symptom Scale-9 Item (baseline)}
#'  \item{mhtrt_0}{MH treatment, past 90 days (baseline)}
#'  \item{sati_0}{Substance Abuse Tx Index (baseline)}
#'  \item{sp_sm_0}{Substance Problem Scale (Past Month) (baseline)}
#'  \item{sp_sm_3}{Substance Problem Scale (Past Month) (3-month follow-up)}
#'  \item{sp_sm_6}{Substance Problem Scale (Past Month) (6-month follow-up)}
#'  \item{gvs}{General Victimization Scale}
#'  \item{ers21_0}{Environment Risk Scale--New (V5)-21 items (baseline)}
#'  \item{nproc}{Count of Treatment A procedures delivered to client (used to be aes_c)}
#'  \item{ada_0}{Adjusted Days Abstinent-Any, baseline}
#'  \item{ada_3}{Adjusted Days Abstinent-Any, recorded at 3-months}
#'  \item{ada_6}{Adjusted Days Abstinent-Any, recorded at 6-months}
#'  \item{recov_0}{Binary indicator indicating if in recovery, baseline}
#'  \item{recov_3}{Binary indicator indicating if in recovery, 3-months}
#'  \item{recov_6}{Binary indicator indicating if in recovery, 6-months}
#'  \item{subsgrps_n}{Categorical variable where: 1="Alcohol and/or marijuana disorder/weekly use; 2="Other drugs"; 3="Opiate disorder/weekly use"}
#'  \item{sncnt}{Total number of sessions for Treatment A}
#'  \item{engage}{Binary indicator indicating initiated treatment and had 4+ sesssions within 45 days for Treatment A}
#' }
#' @source Global Appraisal of Individual Needs biopsychosocial assessment instrument - GAIN - Dennis, Titus et al. 2003
"sud"
