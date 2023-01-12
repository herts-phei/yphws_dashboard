# yphws_dashboard

Project Status: Inactive

# Introduction

The Young People's Health & Wellbeing Survey Public Dashboard shows yearly survey data from the annual Young People's Health & Wellbeing Survey broken down by different groups (e.g. sex, age, ethnicity). The dashboard replaces the [interactive HTML report](https://www.hertshealthevidence.org/documents/yphws/yphws-hertfordshire-report-2021.html). 

### Summary tab

to be written

### Explore Data tab 

Similar to the format of the full 2020 YPHWS Overview Report. to be written

### Inequalities tab 

Tartan rugs. to be written

### Export tab

This tab allows the user to either export a custom/thematic report or export the dataset as a .csv (with filtering).

# Data

- Yearly survey data (currently pinned)
- Yearly question lookups

### Lookup

- question_raw: Question code as specified in SmartSurvey
- question_theme: Section of the survey the question appears in
- question_coded: unique variable name of indicator
- question_text: wording of question as mentioned in sentences in the Differences tables (discontinued?)
- reworded: Sentence structure for Differences tables (discontinued?)
- survey_text: 
- menu_text: text as appeared in multi-category plot dropdown menus
- multi_cat: TRUE if the question has several variables within it (e.g. How often do you have the following foods?)
- multi_binary: TRUE if the question is multi_cat and is binary (e.g. Select all drugs you've been offered > amphetamines, Yes/No)
- question_coded_gen: question_coded, but not unique for multi_cat variables. Used for grouping.
- survey_text_gen:
- response_of_interest: response(s) of interest for each question ("Yes" for "Do you self-harm?")

## Updating the dashboard

During the annual update, make sure to do all operations on the **dev branch** and only merge to master if a thorough QA has been done first.

1. Ensure that the [dashboard-specific processing script](https://hertscc.managed.mango-solutions.com/git/hcc_phei/yphws/yphws_school_report/-/blob/master/R/4_dashboard_data.R) has been run in [yphws_school_report](https://hertscc.managed.mango-solutions.com/git/hcc_phei/yphws/yphws_school_report). Export `stats.rds`, `q_coded.csv`, and `params.rds` from the /outputs folder in that project and import into this project's /data-raw folder.

2. Add the new year as new values in `app.R`, `shinyWidgets::pickerInput("year"...)` and change the `selected` to the latest year as a default.

3. Test every tab and sections within tabs for obvious errors. Make an issue listing any bugs spotted.

4. Once those bugs are resolved, push the changes (updated data, bug fixes) to the dev branch.

5. Detailed QA

6. Push changes to master and redeploy on shinyapps.io and GitHub.

