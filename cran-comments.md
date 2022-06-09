This is a resubmission of the initial CRAN submission of this package.

The update fixes two problems pointed out by Victoria Wimmer (thanks!):

1. Explaining the acronym "SHAP" in the description text. I added the "official" description: "SHapley Additive exPlanations".
2. Added missing "return values" to
  - extractors.Rd: \value
  - is.shapviz.Rd: \value

Running checks identified the following two notes:

- Possibly misspelled words in DESCRIPTION:
    SHAP (2:8, 6:33, 9:22, 13:17)
    SHapley (6:39)
    exPlanations (6:56)

- checking for detritus in the temp directory ... NOTE
  Found the following files/directories:
    'lastMiKTeXException'
