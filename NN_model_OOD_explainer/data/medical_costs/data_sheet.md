# Data Sheet




### Files

The file `insurance.csv` holds all the data. The CSV is separated with `,` (as opposed to other, non-comma legal separators for CSVs). It contains a header line.

### Data Statistics

#### Features

Each record is for one person. There are 7 features, including the prediction variable, the next years health costs.

* **age**: Integer age.
* **sex**: Biological sex, "male" or "female"
* **BMI**: Body mass index, a loose heuristic for predicting over weight, normal weight, or under weight based on height and weight measurements. Popular due to its ease of calculation.
* **children**: The number of children one has. Integer values bounded to 0 or greater.
* **smoker**: "yes" or "no" if individual smokes
* **region**: The region where the individual lives. Options are: "southwest", "southeast", "northwest", and "northeast"
* **expenses**: the annual expenses measured by the expenses billed to health insurance in USD.


#### Instances

1338 instances, with 0 gaps or missing data.


### Source

The data is provided by Cal State East Bay. [https://www.csueastbay.edu/](https://www.csueastbay.edu/). It is hosted on Kaggle at [this link](https://www.kaggle.com/datasets/noordeen/insurance-premium-prediction).
