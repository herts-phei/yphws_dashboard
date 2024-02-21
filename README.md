# YPHWS Dashboard

Project Status: Inactive
This repository contains the source code for the [Public Young People's Health & Wellbeing Survey](https://hcc-phei.shinyapps.io/yphws_dashboard/). 

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

# About the survey

This dashboard shows data from the annual Young People’s Health & Wellbeing Survey (YPHWS) at Hertfordshire level. The Young People’s Health & Wellbeing Survey (YPHWS) is an anonymous online survey which gathers self-reported information annually from those aged 11-19 in Hertfordshire. The survey includes questions about home life, wellbeing, diet, physical activity, smoking, alcohol use, drug use, sexual health, mental health, bullying, and safety. The project is funded by Hertfordshire Public Health and YC Hertfordshire and is run by the Public Health Evidence & Intelligence Team and provides an opportunity for partnership working between organisations providing services to young people around the county.

The survey has been ongoing for two years with good uptake: 12,923 responses in the first year and 11,681 responses in the second. The data in this dashboard is expected to update annually during spring with the latest survey data, collected during November - December in the previous year. Please note that additional questions/indicators may be added in response to health concerns during the time of the survey (e.g. COVID-19). For more information about the survey and supporting reports, please visit the [YPHWS page](https://www.hertshealthevidence.org/yphws/what-is-the-yphws.aspx) on the Herts Health Evidence website. 

If you have queries about the dashboard or the survey data, you can email the Public Health Evidence & Intelligence Team at Ph.intelligence@hertfordshire.gov.uk.

## How to run

Simply clone the project into your local machine and run `app.R`. 

## Structure

The dashboard is separated into 5 main tabs and a link to a feedback survey. To get the most out of the dashboard, we recommend starting by selecting how you want the data to be broken down by using the selection in the navigation bar. This will affect your data views in all tabs. If you select Sex, for example, most graphs and tables will be broken down by All, Female, Male, and Other. Selecting Year group will break the graphs and tables down by Years 7-12, etc.

Note: To protect the identity of respondents, all values are rounded to the nearest 5. This is in line with Office of National Statistics recommendations for disclosure control. Note that this means that when a value constitutes 100% or 0% of responses there may be a small number of individuals responding with a different option who have been suppressed.

The **Key Points** tab shows some general statistics about the respondents and a text summary of the main health topics covered.

![image](https://user-images.githubusercontent.com/79272398/193871356-7dcba617-867f-4c26-8cd5-8d1131c09a29.png)

The **Explore Data** tab shows a graph, trend table, and a data table for each question asked in the survey. (Note: For a full list of the questions asked, you can check the table in the Export tab, or download them in the About tab.) You can navigate to the health topic of your choice using the checkboxes on top.

![image](https://user-images.githubusercontent.com/79272398/193871467-7c7e095e-f61a-4920-882a-f42fada757dd.png)

The **Inequalities** tab shows a "tartan rug" graph coloured by statistical differences between groups. You can use the filters on the left to select the health topic and questions to be shown in the tartan rug. The methodology for these comparisons are the same as those in PHE Fingertips indicaters and can be found in the [PHEindicatormethods package](https://github.com/publichealthengland/PHEindicatormethods) documentation.

![image](https://user-images.githubusercontent.com/79272398/193871597-ec296186-d1d9-4935-9e92-c4157e98a2e4.png)

The **Export** tab allows you to export datasets and download a report version of the full data breakdown. You can use the filters to further customise your exports. Please note that the report may take a minute to generate.

The **About** tab provides further information about the survey, data, and dashboard. 


