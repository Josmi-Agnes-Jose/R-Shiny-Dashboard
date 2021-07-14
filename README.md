# R-Shiny-Dashboard
*Coronary Heart Disease risk prediction using Logistic Regression.*

This is an interactive dashboard based on the Framingham Heart Disease prediction using Logistic regression.
The dashboard is built using the Shiny package in R. The dashboard contains Data Visualizations, details and outcomes of the model built. 

### Code

The code to create and run the application is provided in the `Dashboard.r` R file.You will also be required to use the included `www` folder and the `Framingham_Data.csv` dataset file to completly run the app.  If you are interested in how the visualizations are created in the notebook, please feel free to explore the R file.

### Dataset

The dataset which is used for the logistic regression analysis is available on the Kaggle website, and it is from an ongoing cardiovascular study on residents of the town of Framingham, Massachusetts. The classification goal of this study is to predict whether the patient has 10-year risk of future coronary heart diseases. The Framingham dataset consists of 4240 records of patients. There are 15 features and the target variable, with a total of 645 missing values. This dataset `Framingham_Data.csv`, provided in this repository is the version after pre-processing. The orginal Framingham Heart Study dataset used can be found on the [Kaggle Datasets](https://www.kaggle.com/amanajmera1/framingham-heart-study-dataset).

### www

The `www` directory is the directory expected by a Shiny application to locally store the elements that will be rendered in the web browser and are not the output of the scripts. This includes images (uninteractive elements of the app) that we have used in the code. This directory must be on the application's main folder while running the app locally.

### Report

The `Report.pdf` is a detailed report of the project.

