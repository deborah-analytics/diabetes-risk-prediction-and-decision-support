# Dashboard Applications

This folder contains the Shiny applications developed as part of the project.

## Applications

### `clinical_decision_support_app.R`

A rule-based clinical decision support application built using NICE NG28 guidance for Type 2 Diabetes management.

Main features include:

* patient data entry
* personalised recommendations
* medication and lifestyle guidance
* ICD-10 coding support
* downloadable patient summary
* NICE guideline explanation page

### `diabetes_prediction_app.R`

A machine learning-based prediction tool built using the final Random Forest model from the thesis modelling workflow.

Main features include:

* prediction of No Diabetes, Prediabetes, or Diabetes
* probability-based output
* recommended next steps based on risk level
* downloadable prediction summary
* explanation and guidance modal
* privacy notice and clinical disclaimer

## Relationship Between the Two Apps

The prediction app estimates the likelihood of diabetes status using machine learning.

The clinical decision support app provides rule-based guidance and recommendations aligned with NICE NG28.

Together, the two applications demonstrate both predictive modelling and practical clinical decision support.
