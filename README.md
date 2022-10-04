# YPHWS Dashboard

## About the survey

This dashboard shows data from the annual Young People’s Health & Wellbeing Survey (YPHWS) at Hertfordshire level. The Young People’s Health & Wellbeing Survey (YPHWS) is an anonymous online survey which gathers self-reported information annually from those aged 11-19 in Hertfordshire. The survey includes questions about home life, wellbeing, diet, physical activity, smoking, alcohol use, drug use, sexual health, mental health, bullying, and safety. The project is funded by Hertfordshire Public Health and YC Hertfordshire and is run by the Public Health Evidence & Intelligence Team and provides an opportunity for partnership working between organisations providing services to young people around the county.

The survey has been ongoing for two years with good uptake: 12,923 responses in the first year and 11,681 responses in the second. The data in this dashboard is expected to update annually during spring with the latest survey data, collected during November - December in the previous year. Please note that additional questions/indicators may be added in response to health concerns during the time of the survey (e.g. COVID-19). For more information about the survey and supporting reports, please visit the [YPHWS page](https://www.hertshealthevidence.org/yphws/what-is-the-yphws.aspx) on the Herts Health Evidence website. 

If you have queries about the dashboard or the survey data, you can email the Public Health Evidence & Intelligence Team at Ph.intelligence@hertfordshire.gov.uk.

## How to run

Simply clone the project into your local machine and run `app.R`. 

## Structure

The dashboard is separated into 5 main tabs and a link to a feedback survey. To get the most out of the dashboard, we recommend starting by selecting how you want the data to be broken down by using the selection in the navigation bar. This will affect your data views in all tabs. If you select Sex, for example, most graphs and tables will be broken down by All, Female, Male, and Other. Selecting Year group will break the graphs and tables down by Years 7-12, etc.

Note: To protect the identity of respondents, all values are rounded to the nearest 5. This is in line with Office of National Statistics recommendations for disclosure control. Note that this means that when a value constitutes 100% or 0% of responses there may be a small number of individuals responding with a different option who have been suppressed.

The **Key Points** tab shows some general statistics about the respondents and a text summary of the main health topics covered.

The **Explore Data** tab shows a graph, trend table, and a data table for each question asked in the survey. (Note: For a full list of the questions asked, you can check the table in the Export tab, or download them in the About tab.) You can navigate to the health topic of your choice using the checkboxes on top.

The **Inequalities** tab shows a "tartan rug" graph coloured by statistical differences between groups. You can use the filters on the left to select the health topic and questions to be shown in the tartan rug. The methodology for these comparisons are the same as those in PHE Fingertips indicaters and can be found in the [PHEindicatormethods package](https://github.com/publichealthengland/PHEindicatormethods) documentation.

The **Export** tab allows you to export datasets and download a report version of the full data breakdown. You can use the filters to further customise your exports.

The **About** tab provides further information about the survey, data, and dashboard. 
