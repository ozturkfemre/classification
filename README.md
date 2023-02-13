# classification

Classification Project

## Aim of the Project

In this project, I compared classification tree, bagging, random forest, logistic regression, linear discriminant analysis, quadratic discriminant analysis and support vector machine algorithms on the same data set to compare which one gives better results.

## Dataset Information

The dataset can be accessed in the "kmed" package with the name Heart, or it can be accessed from the [UCI Machine Learning Repository.](https://archive.ics.uci.edu/ml/datasets/Heart+Disease)

There are 14 variables and 297 observations in the data set. Each observation represents a patient. Each variable contains various information of the patient. The table below may be useful to understand what each variable means.

| variable | description                                                                                                                                                                         |
|----------------|--------------------------------------------------------|
| age      | Age in years (numerical).                                                                                                                                                           |
| sex      | Sex: 1 = male, 0 = female (logical).                                                                                                                                                |
| cp       | Four chest pain types: (1) typical angina, (2) atypical angina (3)non-anginal pain, (4) asymptomatic (categorical).                                                                 |
| trestbps | Resting blood pressure (in mm Hg on admission to the hospital) (numerical).                                                                                                         |
| chol     | Serum cholestoral in mg/dl (numerical).                                                                                                                                             |
| fbs      | Fasting blood sugar more than 120 mg/dl (logical).                                                                                                                                  |
| restecg  | Resting electrocardiographic results: (0) normal, (1) having ST-T wave abnormality, (2) showing probable or definite left ventricular hypertrophy by Estes' criteria (categorical). |
| thalach  | Maximum heart rate achieved (numerical).                                                                                                                                            |
| exang    | Exercise induced angina (logical).                                                                                                                                                  |
| oldpeak  | ST depression induced by exercise relative to rest (numerical).                                                                                                                     |
| slope    | The slope of the peak exercise ST segment: (1) upsloping, (2) flat, (3) downsloping (categorical).                                                                                  |
| ca       | Number of major vessels (0-3) colored by flourosopy (numerical).                                                                                                                    |
| thal     | 3) normal, (6) fixed defect, (7) reversable defect (categorical).                                                                                                                   |
| class    | Diagonosis of heart disease (4 classes). It can be 2 classes by setting 0 for 0 values and 1 for non-0 values.                                                                      |

: variable information

**Data Prepration**

-   Before proceeding with the algorithms, the dependent variable was reduced to two levels: 0 for healthy, 1 for heart disease.

-   Although it is numeric, the variable ca, which behaves categorically due to its low range, has been converted to a factor.

-   The data set was divided into 70% train and 30% test data.


## Study


### Descriptive Statistics


