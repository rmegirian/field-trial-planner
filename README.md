# Field Trial Planner

A successful field trial generates data that can support appropriate analyses capable of addressing the research questions the trial is intended to answer.

Not everything that affects the success of a trial or experiment can be controlled. Environmental and other contextual factors can affect what happens during implementation and what data are ultimately obtained. Planning a trial before it is conducted nevertheless provides an opportunity to examine the elements that can be planned in advance and assess whether the different parts of the proposed design are coherent and aligned with what the trial is intended to achieve.

The research question determines what needs to be learned from the trial and constrains the analyses that could appropriately be used to answer it. Because the decisions made before and during data collection shape the resulting data, the experimental design needs to be structured so that the data generated contain the information required for at least one appropriate analytical approach.

| Design decision | What it determines about the data | Why it matters for analysis |
|---|---|---|
| **Treatment factors and levels** | Which conditions are compared and which combinations of conditions are represented | Determines which treatment effects and interactions can be estimated |
| **Randomisation and layout** | How treatments are assigned to experimental units and how observations are related to one another | Determines the experimental unit and the sources of variation that can be separated |
| **Replication** | How many independent experimental units contribute information to each comparison | Affects the ability to estimate variation and the precision of comparisons |
| **Sampling** | Which units are observed, how often, and how observations relate to the experimental units | Determines what each observation represents and whether observations can appropriately be treated as independent |
| **Measurement and timing** | What is measured, on what scale, and when | Determines the type and structure of the resulting response data and what changes can be assessed |
| **Sites, years and other implementations** | How the trial is repeated across environments or conditions | Determines whether and how variation between environments can be assessed |

These decisions are interconnected, with changes in one having flow-on effects on others. There is therefore not necessarily a single right or wrong decision at each stage. Rather, the important question is whether the decisions work together as a coherent overall design that is aligned with the objectives of the trial and capable of producing data that can support the intended analyses.

For example, adding a treatment factor may increase the number of treatment combinations and change the replication required, while constraints on available resources may require the treatment structure, replication or research questions to be reconsidered. Planning provides an opportunity to identify these issues and revise the question or design before the trial is conducted.

Everything that occurs in the lead up to and during data collection determines important properties of the resulting data: what individual observations represent, how observations are related to one another, what sources of variation can be distinguished, and what can subsequently be estimated from the data.

The resulting plan therefore has a purpose beyond guiding implementation. It documents how the data are intended to be generated and provides information that is needed to interpret and analyse those data appropriately. An analyst cannot determine an appropriate analysis from the observed values alone; they also need to understand how those observations were generated through the experimental design and data collection process.

The relationship can therefore be thought of as:

research question ↔ appropriate analysis ↔ experimental design ↔ data structure ↔ valid inference

The Field Trial Planner helps users work through the decisions that shape how a trial will be conducted and, consequently, the structure and information content of the resulting data. It helps users examine whether these elements fit together and identify potential inconsistencies or limitations before the trial is conducted.

# What it does

The planner works through the major components of a field trial design:

trial objective, aim, and research question(s)
treatment factors and their levels
replication, including calculation of residual degrees of freedom to help assess whether the proposed design provides sufficient replication for the intended comparisons
response variables, including their data types, sampling approach and timing
research questions constructed from the factors and responses entered, which can be compared against the stated research questions
repeated implementations of the trial across sites and/or years
a summary of the resulting design

The sections are a way to make a complex task tractable, not a sequence. Treatments, replication, measurements and questions all influence one another, so a decision made in one section may require an earlier decision to be reconsidered.

For example, adding a treatment factor may change the number of treatment combinations and therefore the replication required, while finding that the available resources cannot support the proposed replication may require the research questions or design to be reconsidered.

Experimental design is therefore iterative rather than simply step-by-step. The planner is intended to support this process of developing, checking and revising a proposed trial design.

As users work through the plan, the app checks the information entered against other parts of the proposed design and reports structural features that are relevant to subsequent analysis, including treatments, plots, interactions, replication and restricted randomisation.

These checks are intended to identify potential problems for review. They do not replace statistical or methodological review of the proposed trial.

# Data and outputs

To maintain data privacy, information entered into the planner remains within
the user's browser session. Answers are not written to or stored on a server,
so the app does not retain a copy of the information entered.

When the planning process is complete, the information can be exported as two
files:

- **CSV** — contains the structured responses and can be reloaded into the app
  to continue or revise the plan.
- **HTML summary** — a self-contained summary of the proposed trial design that
  can be shared with collaborators or analysts.

Users are responsible for saving, storing and sharing the exported files in
accordance with the requirements of their organisation and the sensitivity of
the information they contain.

# Status

Under development.

The foundations are complete and tested: data model, configuration, validation, design metrics, CSV round trip and HTML summary.

Licence

Copyright (C) 2026 rmegirian. GNU General Public License v3.0 — see LICENSE.

Free to use, copy, modify and share. If you distribute a modified version, it has to be under the same licence, with source available, so that it stays as open as you found it.
open as you found it.
