# =====================================================================
# AI&DS CGPA / SGPA CALCULATOR -- R Edition (Regulation 2022)
# Ported from index.html (JS web version) into base R.
#
# Same grading scale + same SGPA/CGPA formula as the original app:
#   SGPA = sum(credits * grade_points) / sum(credits)
#   CGPA = sum(SGPA_i * credits_i) across all semesters / total credits
#
# Deliberately built through the exact techniques from Weeks 1-4:
#   Week 1/2 : CSV reading, math ops (sqrt/abs/round), conversions
#   Week 3   : numeric vectors, combining vectors, averages
#   Week 4   : lists, adding to a list, list length, modifying a list
# =====================================================================

# ---- GRADE SCALE (named vector -> lookup table) ----
GRADE_POINTS <- c(O = 10, `A+` = 9, A = 8, `B+` = 7, B = 6, C = 5, U = 0)

# ---------------------------------------------------------------------
# 1. READ SUBJECT DATA FROM CSV  (Week 1a, 4a: CSV -> data -> list)
# ---------------------------------------------------------------------
read_semester_subjects <- function(csv_path, semester_no) {
  all_subjects <- read.csv(csv_path, stringsAsFactors = FALSE)
  sem_data <- all_subjects[all_subjects$Semester == semester_no, ]
  if (nrow(sem_data) == 0) {
    stop("No subjects found for semester ", semester_no)
  }
  rownames(sem_data) <- NULL
  as.list(sem_data)   # Week 4a: convert CSV rows into a list
}

# ---------------------------------------------------------------------
# 2. ENTER / SIMULATE GRADES -> build a character VECTOR (Week 3)
#    (interactive mode uses readline; demo mode uses supplied grades)
# ---------------------------------------------------------------------
enter_grades <- function(subjects, demo_grades = NULL) {
  n <- length(subjects$Code)
  valid <- names(GRADE_POINTS)

  if (!is.null(demo_grades)) {
    if (length(demo_grades) != n) stop("demo_grades length must match number of subjects")
    return(toupper(demo_grades))
  }

  grades <- character(n)
  cat("\nEnter grade for each subject (", paste(valid, collapse = ", "), "):\n", sep = "")
  for (i in seq_len(n)) {
    repeat {
      cat(sprintf("[%s] %s (Credits: %d): ",
                   subjects$Code[i], subjects$Subject[i], subjects$Credits[i]))
      g <- toupper(trimws(readline()))
      if (g %in% valid) { grades[i] <- g; break }
      cat("  Invalid grade. Try again.\n")
    }
  }
  grades
}

# ---------------------------------------------------------------------
# 3. COMPUTE SGPA  (Week 3e: weighted average over vectors)
# ---------------------------------------------------------------------
compute_sgpa <- function(credits, grades) {
  points <- unname(GRADE_POINTS[grades])   # vector lookup, Week 3a-style extraction
  total_points  <- sum(points * credits)
  total_credits <- sum(credits)
  round(total_points / total_credits, 2)   # Week 1b-style rounding/math
}

# ---------------------------------------------------------------------
# 4. COMPUTE CGPA across semesters (Week 3c: combine multiple vectors)
# ---------------------------------------------------------------------
compute_cgpa <- function(sgpa_vector, credits_vector) {
  total_points  <- sum(sgpa_vector * credits_vector)
  total_credits <- sum(credits_vector)
  round(total_points / total_credits, 2)
}

# ---------------------------------------------------------------------
# 5. CLASSIFY PERFORMANCE (same bands as the web app's getClass())
# ---------------------------------------------------------------------
classify_cgpa <- function(cgpa) {
  if (cgpa >= 9)   return("Outstanding")
  if (cgpa >= 8)   return("First Class with Distinction")
  if (cgpa >= 6.5) return("First Class")
  if (cgpa >= 5)   return("Second Class")
  return("Below Passing")
}

# ---------------------------------------------------------------------
# 6. ADD A SEMESTER RECORD TO A LIST (Week 4c/4d: build & modify a list)
# ---------------------------------------------------------------------
add_semester_record <- function(record_list, semester_no, sgpa, credits) {
  record_list[[paste0("Sem", semester_no)]] <- list(SGPA = sgpa, Credits = credits)
  record_list
}

# ---------------------------------------------------------------------
# 7. EXPORT REPORT TO CSV + report list length (Week 4e)
# ---------------------------------------------------------------------
export_report <- function(record_list, out_path) {
  df <- data.frame(
    Semester = names(record_list),
    SGPA     = sapply(record_list, function(x) x$SGPA),
    Credits  = sapply(record_list, function(x) x$Credits),
    row.names = NULL
  )
  write.csv(df, out_path, row.names = FALSE)
  cat("\nSemesters recorded (list length):", length(record_list), "\n")
  cat("Report saved to:", out_path, "\n")
  df
}

# =======================================================================
# MAIN DRIVER
# csv_path     : path to semesters.csv
# sem_no       : current semester (1-8)
# demo_grades  : optional vector of grades, e.g. c("O","A+","A","B+", ...)
#                matching the subject count for that semester -- lets the
#                script run non-interactively (via Rscript) for lab demos
# prev_sgpas   : optional named/plain numeric vector of SGPA for all
#                semesters before sem_no, e.g. c(8.5, 7.9) for sems 1 & 2
# =======================================================================
run_calculator <- function(csv_path = "semesters.csv",
                            sem_no,
                            demo_grades = NULL,
                            prev_sgpas = NULL) {

  cat("========================================\n")
  cat(" AI&DS CGPA / SGPA CALCULATOR (R Edition)\n")
  cat("========================================\n")

  subjects <- read_semester_subjects(csv_path, sem_no)
  grades   <- enter_grades(subjects, demo_grades)
  sgpa     <- compute_sgpa(subjects$Credits, grades)

  record <- list()
  all_sgpa    <- sgpa
  all_credits <- sum(subjects$Credits)

  if (sem_no > 1) {
    if (is.null(prev_sgpas) || length(prev_sgpas) != sem_no - 1) {
      stop("Provide prev_sgpas as a numeric vector of length ", sem_no - 1)
    }
    prev_credits <- numeric(sem_no - 1)
    for (i in 1:(sem_no - 1)) {
      prev_subj      <- read_semester_subjects(csv_path, i)
      prev_credits[i] <- sum(prev_subj$Credits)
      record <- add_semester_record(record, i, prev_sgpas[i], prev_credits[i])
    }
    all_sgpa    <- c(prev_sgpas, sgpa)
    all_credits <- c(prev_credits, sum(subjects$Credits))
  }

  record <- add_semester_record(record, sem_no, sgpa, sum(subjects$Credits))
  cgpa   <- compute_cgpa(all_sgpa, all_credits)

  cat("\n--------- RESULT ---------\n")
  cat(sprintf("Semester %d SGPA : %.2f\n", sem_no, sgpa))
  cat(sprintf("Overall CGPA     : %.2f\n", cgpa))
  cat(sprintf("Classification   : %s\n", classify_cgpa(cgpa)))
  cat("---------------------------\n")

  export_report(record, "cgpa_report.csv")
  invisible(list(sgpa = sgpa, cgpa = cgpa, record = record))
}

# =======================================================================
# DEMO (runs non-interactively -- exactly what you'd show in a viva)
# Simulates Semester 3 (8 subjects) with SGPA already known for Sem 1 & 2
# =======================================================================
if (sys.nframe() == 0) {
  demo_grades_sem3 <- c("O", "A+", "A", "B+", "O", "A+", "A", "O")
  run_calculator(
    csv_path    = "semesters.csv",
    sem_no      = 3,
    demo_grades = demo_grades_sem3,
    prev_sgpas  = c(8.4, 8.1)   # Sem 1 = 8.4, Sem 2 = 8.1
  )
}
