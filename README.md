# Field Trial Planner
The Field Trial Planner helps users work through the decisions that shape how a trial will be conducted and, consequently, the structure and characteristics of the resulting data. It helps users examine whether these elements are aligned with an appropriate analytical approach and identify potential inconsistencies or limitations before the trial is conducted. Because this information is important for understanding how the data were generated and determining an appropriate analysis, the completed plan can also, provided that it accurately reflects what took place, help guide the analysis of the resulting data.

## Why planning matters for analysis
A field trial should be designed to generate data that can support appropriate analyses addressing the research questions the trial is intended to answer.

Not everything that affects the success of a trial or experiment can be controlled. Environmental and other contextual factors can affect what happens during implementation and the data that are ultimately generated. Planning a trial before it is conducted nevertheless provides an opportunity to examine the elements that can be planned in advance and assess whether the different parts of the proposed design are coherent and aligned with what the trial is intended to achieve.

The research question determines what needs to be learned from the trial and constrains the analyses that could appropriately be used to answer it. Because the decisions made before and during data collection shape the resulting data, the experimental design needs to be structured so that the data generated contain the information required for at least one appropriate analytical approach.

| Design decision | What it determines about the data | Why it matters for analysis |
|---|---|---|
| **Treatment factors and levels** | Which conditions are compared and which combinations of conditions are represented | Determines which treatment effects and interactions can be estimated |
| **Randomisation and layout** | How treatments are assigned to experimental units and how experimental units are arranged within the trial | Determines the experimental unit, the structure of treatment comparisons, and sources of variation that can be accounted for |
| **Replication** | How many independent experimental units contribute information to each comparison | Affects the ability to estimate variation and the precision of comparisons |
| **Sampling** | Which units are observed, how observations are taken, and how observations relate to the experimental units | Determines what each observation represents and whether observations can appropriately be treated as independent |
| **Measurement and timing** | What is measured, on what scale, and when | Determines the type and structure of the response data, which affects the appropriate model family, error structure, and changes that can be assessed |
| **Sites, years and other implementations** | How the trial is repeated across environments or conditions | Determines whether and how variation between environments can be assessed |

These decisions are interconnected, with changes in one having flow-on effects on others. Experimental design is therefore not a linear checklist of items with a single correct decision at each stage. Rather, it is an iterative process of identifying potential constraints, considering trade-offs, and refining the research question, analytical objectives, and design as needed to achieve a coherent and feasible overall design. The important question is whether these decisions work together in a way that is aligned with the objectives of the trial and capable of producing data that contain the information needed to address the research question through an appropriate analysis.

The relationship can therefore be thought of as:

research question ↔ analytical objectives ↔ experimental design ↔ data structure ↔ statistical analysis ↔ valid inference

## What it does
The Planner's core function is to support the planning and review of **trial design structure and consistency** by working through the major components of a field trial design.

### Design structure
- Trial objective, aim, and research question(s)
- Treatment factors and combinations
- Experimental units
- Replication
- Blocking and restricted randomisation
- Repeated measurements
- Response variables, including their data types, sampling approach, and timing
- Repeated implementations of the trial across sites and/or years

These sections make a complex design task tractable while allowing the relationships between decisions to be considered. For example, adding a treatment factor may increase the number of treatment combinations and change the replication required, while finding that available resources cannot support the proposed replication may require the research questions or design to be reconsidered.

As users work through the plan, the app checks information entered in one part of the design against other parts, calculates and reports derived structural features relevant to subsequent analysis, such as the number of treatment combinations and residual degrees of freedom, and constructs research questions from the factors and responses entered so that these can be checked alongside the stated research questions and produces a summary of the proposed design.

These checks are intended to identify potential issues for review. They do not replace statistical or methodological review of the proposed trial, which may still be required where the design, response structure, or assumptions are complex.

## Data and outputs
To maintain data privacy, information entered into the Planner remains within the user's browser session. Answers are not written to or stored on a server, so the app does not retain a copy of the information entered.

Users are advised to download their plan as they work to avoid losing information if the browser session is closed or interrupted. The downloaded CSV can be reloaded into the app at any time to continue or revise the plan.

When the planning process is complete, the plan can be exported as:

**CSV** - a machine-readable version of the structured responses for saving, reloading, or further processing.
**HTML summary** - a self-contained summary of the proposed trial design that can be shared with collaborators or analysts.

Users are responsible for saving, storing, and sharing exported files in accordance with the requirements of their organisation and the sensitivity of the information they contain.

## Status
Under development.

The foundations are complete and tested: data model, configuration, validation, design metrics, CSV round trip and HTML summary.

## Licence

Copyright (C) 2026 rmegirian. GNU General Public License v3.0 — see LICENSE.

Free to use, copy, modify and share. If you distribute a modified version, it has to be under the same licence, with source available, so that it stays as open as you found it.
open as you found it.
