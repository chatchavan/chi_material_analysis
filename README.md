
# Data export settings from LimeSurvey

* Completion state: Completed responses only
* Export responses as: Answer code
* Export questions as: Question code & question text


# Global settings

See `r/constants.R`

* `should_exclude_mismatch`: set to TRUE to exclude materials that the expertise mismatch (Exclusion example: a response about “study material” type but the respondent indicate that he/she is not responsible for designing the study)


# Analysis scripts

* `r/_run_all_analysis.R` to run all files below
* `r/availability.R` for RQ1
* `r/private.R` for RQ2
* `r/public.R` for RQ3
