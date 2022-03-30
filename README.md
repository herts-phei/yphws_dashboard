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
