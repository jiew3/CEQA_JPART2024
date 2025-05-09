# Agency consultation networks in environmental impact assessment

This repository is for a paper published in the Journal of Public Administration Research & Theory

This repository contains all of the data and code materials necessary to replicate the project. For questions, please email us.

R version 4.2.2 (2022-10-31)

# Paper Abstract
Government agencies practice inter-agency consultation to ensure that broader governmental activities align with their missions and objectives. Consultation allows agencies to express their preferences and interests, but also may create administrative burden and procedural delay. To explore the conditions under which agencies choose to review activities proposed by fellow government actors, this research focuses on the California Environmental Quality Act (CEQA), California’s environmental impact assessment law. We conceptualize the CEQA review network as a two-mode network, in which each review agency is linked to particular projects, and use two-mode exponential random graph models to test a series of hypotheses about agency, project, and agency-project dyadic characteristics that shape the choice to review. Interestingly, we find that projects located in sites with socioeconomically vulnerable residents or higher levels of background pollution garner more consultation. Likewise, agencies are more likely to provide consultation when their expertise aligns with the project’s impact, and are less likely to review a project with agencies that possess the same expertise. This research highlights variations underlying interagency consultation and helps understand how agencies try to influence other agencies’ decisions.

# Directory items
The directory has 4 folders with the following files:

code/ scripts for replicating the main manuscript

- code.R

input/ datasets for the main manuscript

- Attributes_all.rds: node attributes

- review_net_all.rds: agency-project relationship --> used to set up a network structure

Figures/

- Figures and Table A2 included in the paper


TableA4/ 

- datasets and code used for producing Table A4: Analysis of the CEQA Review Network with Regional Agencies Collapsed into a Single Large Agency
- same code, with changes in datasets

#  Authors
Contact info

Jie Wang
jiew35@uci.edu

------[Updated by Jie Wang on 05/08/2025]
Many thanks to Dr. Santiago Quintero Suarez for identifying and correcting a coding error in my original analysis for testing multicollinearity. The code and results have been revised accordingly. You can find the revised Table A2 under the Figures folder. Based on revised VIF analysis, this research also did not find evidence of multicollinearity in the ERGM model, and the potential risk of multicollinearity becomes even lower.



