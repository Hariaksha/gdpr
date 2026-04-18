# GDPR & Firm Behavior

Empirical research on the impact of the EU's General Data Protection Regulation (GDPR) on company behavior, with a focus on how regulatory uncertainty and compliance costs have shaped firm-level innovation, data practices, and workforce adjustments.

> **Collaborators:** Hari Gunda, Dr. Martin Kesternich (ZEW)

---

## Overview

The GDPR, which came into force in May 2018, represents one of the most significant data governance interventions in modern regulatory history. Despite its scale and scope, the causal effects of GDPR on firm behavior remain empirically underexplored. This project uses panel data methods and program evaluation tools to estimate how firms responded to GDPR — particularly under conditions of regulatory uncertainty during the lead-up to and immediate aftermath of enforcement.

---

## Research Questions

The core questions driving this project are:

- **Compliance costs:** How much did GDPR increase compliance costs for European firms, and did these costs differ by firm size, sector, or data intensity?
- **Innovation:** Did GDPR reduce or redirect firm-level innovation activity, particularly in data-driven sectors?
- **Data practices:** How did firms adjust their data collection, storage, and sharing practices following GDPR enforcement?
- **Workforce:** Did firms adjust hiring, staffing, or organizational structure in response to the regulation?
- **Regulatory uncertainty:** Does uncertainty about GDPR interpretation and enforcement — before and after May 2018 — independently affect firm behavior beyond the regulation itself?

---

## Methodology

The project combines firm-level survey data with econometric program evaluation methods to identify causal effects of GDPR on firm outcomes.

### Empirical approaches

- **Difference-in-differences (DiD):** Comparing firms more and less exposed to GDPR requirements before and after the enforcement date
- **Event study designs:** Examining how firm behavior changed around the May 2018 enforcement date
- **Heterogeneity analysis:** Testing whether effects differ by firm size, industry, data intensity, and country
- **Regulatory uncertainty measures:** Constructing proxies for policy uncertainty during the pre-enforcement period

### Data

The project draws on firm-level survey data and related datasets to measure compliance behavior, innovation outcomes, and firm responses to GDPR. See the `data/` folder for the datasets used in the analysis.

---

## Repository Structure

```text
gdpr/
├── code/
│   └── firm-response-analysis.ipynb   # Main analysis notebook
├── data/                               # Firm-level and regulatory datasets
├── publications/                       # Posters, slides, and presentation outputs
├── reports/                            # Internal reports and documentation
├── surveys/                            # Survey instruments and related materials
├── results.docx                        # Summary of results
└── README.md
```

---

## Contents

### `code/`
Contains the primary analysis notebook (`firm-response-analysis.ipynb`), where empirical models are built and estimated. The repository uses a mix of Jupyter Notebooks (Python), R, and HTML-rendered outputs.

### `data/`
Houses firm-level datasets and regulatory data used in the analysis pipeline.

### `publications/`
Contains research outputs including conference posters. This project was presented at the UA Undergraduate Research & Creative Activities Conference in April 2026 under the title *"The Economics of Data Privacy: Regulatory Uncertainty and Corporate Responses to the EU GDPR."*

### `reports/`
Internal reports and project documentation developed throughout the research process.

### `surveys/`
Survey instruments and materials related to firm-level data collection.

### `results.docx`
A working document summarizing current empirical findings and model outputs.

---

## Conference Presentations

| Venue | Title | Date |
|---|---|---|
| UA Undergraduate Research & Creative Activities Conference | The Economics of Data Privacy: Regulatory Uncertainty and Corporate Responses to the EU GDPR | April 2026 |

---

## Applications

The findings from this project have practical relevance across research, policy, and industry:

- **Regulatory design:** Policymakers can use evidence on compliance costs and uncertainty effects to design more graduated, predictable data governance frameworks.
- **Innovation policy:** Results bearing on whether GDPR dampened R&D or redirected investment can inform future EU technology and industrial policy.
- **Corporate strategy:** Firms operating in or entering EU markets can benchmark their compliance postures against empirically measured firm responses.
- **Digital governance:** Results contribute to global debates on how data protection regulation shapes the digital economy, particularly relevant as GDPR-inspired laws spread to the US, Brazil, and beyond.
- **Academic research:** The project provides a replicable econometric template for studying the firm-level effects of major regulatory interventions.

---

## Tools & Languages

| Tool | Use |
|---|---|
| Python (Jupyter Notebook) | Main empirical analysis |
| R | Supporting statistical analysis |
| HTML | Report rendering |

---

## Project Status

Active research. The `code/` folder was last updated in April 2026 and the results document is under active revision.

---

## Authors

**Hariaksha Gunda**
University of Alabama · Visiting Researcher, PARSEC Paderborn (Summer 2025)

Dr. Martin Kesternich
ZEW

Repository: [https://github.com/Hariaksha/gdpr](https://github.com/Hariaksha/gdpr)

---
