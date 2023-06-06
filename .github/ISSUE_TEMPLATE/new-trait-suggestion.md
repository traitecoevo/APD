---
name: New trait suggestion
about: Make a suggestion for a new trait
title: '["Trait suggestion"]'
labels: ''
assignees: ehwenk

---

body:
- type: markdown
  attributes:
    value: "Thanks for taking the time to propose a new trait concept to add to the APD."
- type: input
  id: contact
  attributes:
    label: Contact Details
    description: What are your contact details?
    placeholder: ex. email@example.com
  validations:
    required: false
- type: input
  id: trait label
  attributes:
    label: trait label
    description: What is an appropriate label (name) for the trait you are suggesting?
    placeholder: value for trait label
    value: "seed dry mass"
  validations:
    required: true
- type: textarea
  id: trait description
  attributes:
    label: trait description
    description: Provide a 1-2 sentence description for your trait, indicating explicitly what plant part is being measured and what characteristic of the plant part is being measured.
    placeholder: value for trait description
    value: "Dry mass of a mature seed, including both oven dried and air-dried samples."
  validations:
    required: true
- type: dropdown
  id: trait type
  attributes:
    label: trait type
    description: Is this a categorical or numeric trait?
    options:
      - categorical
      - numeric
  validations:
    required: true
- type: textarea
  id: preferred units
  attributes:
    label: preferred units
    description: For numeric traits, what are the preferred units for this trait?
    placeholder: units
    value: "mg"
  validations:
    required: false
- type: textarea
  id: allowable categorical values
  attributes:
    label: allowed categorical values 
    description: For categorical traits, please propose a list of allowable values and definitions for the values.
    placeholder: units
    value: " "
  validations:
    required: false
- type: textarea
  id: references
  attributes:
    label: key references
    description: Please list key references for this trait. These may include trait handbooks, other trait databases where the trait is defined, the first publication to suggest this trait, or other references championing this trait or providing an outstanding summary of its relevance.
    placeholder: references
    value: "doi.org/.... or author date title"
  validations:
    required: false 
- type: textarea
  id: extras
  attributes:
    label: additional information
    description: If you have additional information to provide about this trait, please do so here. This may include keywords, mentions of the trait in other databases, or allowable ranges (for numeric traits).
    value: "value range should be 1-100"
  validations:
    required: false
