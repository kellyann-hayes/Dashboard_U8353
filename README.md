# Dashboard_U8353

Link to app:  https://9zj30i-kellyann-hayes.shinyapps.io/Dashboard_U8353/ 

This app provides a cursory look at the NYPD’s School Safety Data (available here) from 2019 to 2022. 

## Background: The Student Safety Act

(small amount of research on cops in schools, Black and Latinx students school to prison pipeline, etc, events leading up to Student Safety Act)

The Student Safety Act went into effect in New York City at the start of the 2019-2020 school year, following growing criticism from communities and advocates over the role of School Safety Agents (SSA), civilian NYPD employees stationed in schools, in criminalizing student behavior and contributing to the school to prison pipeline for Black and brown youth.
 
In addition to allowing students to file complaints with the Civilian Review Complaint Board (CCRB), the Student Safety Act mandates quarterly reporting from the NYPD on School Safety Agent (SSA) and other NYPD interaction with students on NYC school campuses. Advocates hoped that these measures would help to ensure transparency and accountability, and help New Yorkers 

In 2019, the NYCLU published a fact sheet analyzing trends in the NYPD’s “School Safety Data” from 2016 to 2019, finding troubling racial disparities. This dashboard app aims to make the data for the years since accessible for quick consumption. The visualizations made available through the dashboard do not show the full picture of the data, but due to file size restrictions further exploration was limited; future iterations of the dashboard could better synthesize the School Safety Data.

## Data and Methods 

From the NYPD School Safety Data, quarterly reports by precinct and by school available as Excel files, from 2019 to 2022, were used to create the visualizations in this app. School-level and precinct-level datasets, each observation is a single incident of contact with a student. After cleaning, these datasets were merged with NYC public school location and NYPD precinct shapefiles available at NYC Open Data for mapping. 

While data cleaning was extensive and I will not discuss here the process at length (full code is available in the repository), below I outline some considerations of note while using this app: 

*School-Level School Safety Data*
<ol>
  <li>*No Demographics*: The school-level data does not include student demographics such as race, gender, and age. Exploration of demographics must be done at the precinct level. </li>
  <li>*Difficulties Linking Schools to Shapefiles*: School names did not include a unique identifier, and the coded values were inconsistent with the names of schools in the NYC public schools shapefile available at NYC Open Data. To match schools to their coordinate locations, first schools with “PS” or “IS” codings were extracted from both the NYPD data and the shapefile for matching. Then, schools without “PS” or “IS” codings were matched via “fuzzy merge”. Finally, 133 schools were manually matched. This still left some schools unmatched. Due to difficulties with matching the schools to their coordinates, the matched data, including the ATS_CODE code which can be matched to NYC public school shapefiles as a unique identifier, is available for download as a .csv file in the “Map” tab of the app. </li>
</ol>

*Precinct-Level School Safety Data*
<ol>
  <li>*Race variable re-coding*: Some race categories were disaggregated (e.g., “East Indian” or “Arabic”) but were used sparingly in the dataset; because it was unclear with what consistency a, for example, Indian student would have been categorized into the “East Indian” category rather than the “Asian/Pacific Islander” category (per the census aggregation), the race field was re-coded into the 5 main race categories provided by the census.
    <ol>
      <li>American Indian</li>
      <li>Asian/Pacific Islander</li>
      <li>Black</li>
      <li>Hispanic/Latinx</li>
      <li>White</li>
    </ol>
 </li>
</ol>

## Using the App

#### Map 

The ‘Map’ tab of the app allows users to explore the variation of total reported interactions with students by NYPD precinct or by school. The underlying shading of the precincts reflects the total amount of interactions within that precinct, with darker blue precincts having more interactions. The green circles represent schools, with circle size based on the amount of NYPD interventions. Users can choose to look at either the precincts alone, the schools alone, or both by toggling the “Choose a Dataset” options to the left of the map. A specific year from 2019 - 2022 can also be selected. 

Users can also search for a specific school (please note that not all schools will be available on the map), and can change the size of the circles and the shading of the precincts to reflect a specific interaction type (e.g., arrests) rather than the default total number of interactions. 
Finally, as previously discussed a .csv file download is available to facilitate matching the schools reported by the NYPD in their School Safety Data to NYC Public School shapefiles. 

#### Incident Summary

The “Incident Summary” tab of the app allows users to explore changes in types of interactions reported with students, and changes in the agency/department conducting the enforcement (e.g., the NYPD Detective Bureau or SSAs) from 2019 - 2022. Differences in these trends can be explored by borough and by use of restraints during the interaction. 

#### Demographics

The “Demographics” tab of the app allows users to explore racial, gender, and age trends in school safety intervention types from 2019 - 2022. Trends can be examined city-wide or for a specific police precinct. 
