# 2023-Statistical-Risk-Assessment
Replication files for the 2023 edition of USHMM's statistical risk assessment.

You should use the data stored in the repo, "prepared2022predictors-2023-10-26-2.csv". This is preferable to regenerating the data yourself, since doing so requires a host of ancillary files, pulling data from online sources, etc. We nevertheless include a folder containing the R files used to make the data (Make Data), for reference and replicability. Within this folder:

"basedata2023_13sept2023" is a CSV file that contains our initial country-year template and codes the dependent variable through 2022.
"recreatebasedata2023" is an R Markdown file that appends the new country-year template to prior EWP data, from the 2021 update ("prepared2021predictors-2022-09-30.csv").
"Adding in new data-2023 update" is an R Markdown file that appends data for each country in 2022 to the prepared data through 2021.
The input folder contains the data prepared through 2021 from last year's analysis: "prepared2021predictors-2022-09-30.csv"
The output folder contains the prepared data, updated through 2022: "prepared2022predictors-2023-10-26-2.csv"
The output folder also contains an updated file with just the mass killing variables: "mkl_data.csv"
The main files of interest are in the Modelling folder.

**"run model" ** is an R Markdown file that takes the latest data ("prepared2022predictors-2023-10-26-2.csv") and generates forecasts. It allows you to input a vector of base years, from which you would like to generate one and two-year forecasts. The most relevant forecasts have been included in the results folder, generated from base year 2022, predicting outcomes in the next year (2023) and in the next two years (2023 and 2024). **Note that if you choose to run the model for base years earlier than 2016, there is some missingness in the data that will prevent you from getting predicted probabilities for all 162 of the countries [Chad- not sure if this last sentence still applies].**
**"base2021-run-2022-09-30"** and **"base2021-coeffs-run-2022-09-30"** are the outputs of the **"run model" **file. The former file shows the predicted probabilities for each country, and the latter shows the weights for each of the predictors selected.

--
