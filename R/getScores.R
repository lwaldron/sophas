#' Title Get GPAs and GRE scores from a SOPHAS full application PDF
#'
#' @param pdfdir A full-specified path to a directory containing one or more
#' SOPHAS application PDFs
#'
#' @return a data.frame with applicants in rows and data in columns
#' @export
#' 
#' @examples  
#' \dontrun{
#' pdfdir <- "~/applications"
#' x=getScores(pdfdir)
#' rubric <- data.frame(
#'   quantitative = as.integer(cut(x$`Quantitative`, c(0, 145, 148, 154, 1000), labels=1:4)),
#'   verbal = as.integer(cut(x$Verbal, c(0, 155, 160, 163, 1000), labels=1:4)),
#'   analytical = as.integer(cut(x$Analytical, c(0, 3.9, 4.4, 4.9, 10), labels=1:4)),
#'   gpa = as.integer(cut(x$`Cumulative Undergraduate` , c(0, 3, 3.299, 3.599, 4.01), labels=1:4)))
#' rownames(rubric) = rownames(x)
#' gpacols <- c( (grep("Graduate", colnames(x))+1):grep("Overall", colnames(x)), 
#'               grep("Cumulative", colnames(x)))
#' grecols <- match(c("Quantitative %", "Verbal %", "Analytical %"), colnames(x))
#' final = cbind(rubric, x[, c(grecols, gpacols)])
#' }
#' @importFrom methods is
getScores <- function(pdfdir){
  files <- dir(pdfdir, pattern = "pdf$", full.names = TRUE)
  pdftexts <- sapply(files, pdftools::pdf_text)
  nms <- getNAME(pdfdir)
  gpas <- lapply(pdftexts, function(x) getGPA(x))
  gpas <- do.call(rbind, gpas)
  gres <- lapply(pdftexts, function(x) getGRE(x))
  gres <- do.call(rbind, gres)
  res <- cbind(gpas, gres)
  rownames(res) <- make.unique(getNAME(pdfdir))
  for (i in grep("GRE Date", colnames(res), invert=TRUE)){
    res[[i]] <- as.numeric(res[[i]])
  }
  gpa <- rowMeans(res[, (grep("Graduate", colnames(res))+1):grep("Overall", colnames(res))], na.rm = TRUE)
  gres <- res$`Quantitative %` + res$`Analytical %` + rank(res$`Verbal %`)
  res <- res[order(rank(gpa) + rank(gres, na.last=FALSE), decreasing = TRUE), ]
  return(res)
}

getGRE <- function(pdftext) {
    lines <- strsplit(pdftext, "\n")
    lines <- unlist(lines)
    gregex <- "([0-9]{2}-[0-9]{2}-20[0-9]{2})\\s+[0-9]*\\s+([0-9]{3})\\s([0-9]{1,2})%\\s+([0-9]{2,3})\\s([0-9]{1,2})%"
    grelines <- grep(gregex, lines, value = TRUE)
    gre <- strsplit(gsub(" +", "z", grelines), "z")
    if(length(gre) == 0){
        gre <- list(rep("none", 8))
    }
    for (i in seq_along(gre)){
        lose <- grepl("[0-9]{5,}", gre[[i]]) & !grepl("-", gre[[i]])
        gre[[i]] <- gre[[i]][!lose]
    }
    names(gre[[1]]) <-
        c(  "",
            "GRE Date",
            "Verbal",
            "Verbal %",
            "Quantitative",
            "Quantitative %",
            "Analytical",
            "Analytical %"
        )
    gre <- do.call(rbind, gre)[, -1]
    gre <- sub("%", "", gre)
    if(is(gre, "character")){
        gre <- t(gre)
    }
    gre <- data.frame(gre, stringsAsFactors = FALSE, check.names=FALSE)
    gre <- gre[nrow(gre), , drop=FALSE]
    return(gre)
}

getGPA <- function(pdftext) {
    lines <- strsplit(pdftext, "\n")
    lines <- unlist(lines)
    cnames <-
        c(
            "Business",
            "Health Science",
            "Math/Statistics",
            "Other",
            "Public Health",
            "Social/Behavioral",
            "Biology/Chemistry/Physics/Life Science",
            "Lower Division",
            "Upper Division",
            "Post-Baccalaureate",
            "Cumulative Undergraduate",
            "Graduate",
            "Professional",
            "Non-Traditional Medicine",
            "Undergraduate",
            "Overall"
        )
    cug <- grep("    Subject    |     Year     ", lines, value = TRUE)
    if (length(cug) == 0) {
        gparegex <- "[a-zA-Z]+\\s{15,}[23]\\.[0-9]{2}\\s{15,}[0-9]{2,}\\.[0-9]"
        cug <- grep(gparegex, lines, value = TRUE)
        cug <- grep(paste(cnames, collapse = "|"), cug, value = TRUE)
        cug <- strsplit(gsub("[ ]{5}\\s+", "z", cug), "z")
        cug <- do.call(rbind, cug)
        cug[, 1] <- sub("\\s+", "", cug[, 1])
        cug <- cug[match(cnames, cug[, 1]),]
        cug.gpa <- as.numeric(cug[, 2])
    } else{
        cug <- grep(paste(cnames, collapse = "|"), cug, value = TRUE)
        cug <- strsplit(gsub("[ ]{5}\\s+", "z", cug), "z")
        cug <- cug[sapply(cug, length) == 4]
        cug <- do.call(rbind, cug)
        cug[, 1] <- sub("\\s+", "", cug[, 1])
        cug <- cug[match(cnames, cug[, 1]), ]
        cug.gpa <- as.numeric(cug[, 3])
    }
    if(length(cug.gpa) == 0)
        cug.gpa <- rep(NA, length(cnames))
    names(cug.gpa) <- cnames
    cug.gpa <- data.frame(t(cug.gpa), check.names = FALSE)
    return(cug.gpa)
}

getNAME <- function(pdfdir){
    nms <- dir(pdfdir)
    nms <- sub("^[0-9]+ ", "", nms)
    nms <- sub(" - .+", "", nms)
    return(nms)
}
