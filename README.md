# Households and living arrangements

Visualization dashboard (R & Shiny) for the _United Nations (UN)_ and the _Centre for Demographic Studies (CED)_

**About data**
Data presented in this site has been produced in the context of a colLabourative project between the Center for Demographic Studies (Barcelona) and United Nations .


**Sources**

Households level variables
1. IPUMS-International (IPUMS)
We have used all the IPUMS samples where persons are organized into households.

Samples where persons were not organized into household were excluded.

We have included private households only.We have found irregular values for Puerto Rico 1990 in households with at least one member aged 60+.

2. LABOUR FOURCE SURVEY (LFS)
We have included private households only.

We have selected the 2011 and 2001 samples. In 2011 there was household level information in 24 European countries. In 2001 there were 23 countries.

We have Lithuania 2002, Germany 2005 and Ireland 2006 as the earliest data points. No data are available for Lithuania and Ireland before 2002 and 2006 respectively. In the case of Germany, before 2005, there was a problem with the variables used to link household members between them. Therefore, we decided to use Germany 2005.

Sweden, Norway, Denmark, Switzerland do not provide HH level information like HHLINK, HHSPOU, HHFATH, HHMOTH. Therefore, they are excluded from the analysis.

We have relatively high values for some countries in the variable HH with a female head (e.g in Latvia, Luxembourg, Ireland, Estonia, Lithuania and Ireland the values are above 0.55). This can be due to the selection of the household head (reference person) in the LFS

3. DEMOGRAPHIC AND HEALTH SURVEYS (DHS)

Individual level variables
1. IPUMS-International (IPUMS)
We have used all the IPUMS samples where persons are organized into households. Samples where persons were not organized into household were excluded.

We have included private households only.

The last age group with data for each country is the open group. For example, in Argentina 1970 the last age group is 95+.

We have detected a problem in the Spanish Census 2011: the original pointers show that there are are some children (0-14) with own children in the household (5-6% of the 0-14 y.o. population). This is from the original data, we cannot do anything.

2. LABOUR FOURCE SURVEY (LFS)
We have included private households only.

We have selected the 2011 and 2001 samples. In 2011 there was household level information in 24 European countries. In 2001 there were 23 countries.

We have Lithuania 2002, Germany 2005 and Ireland 2006 as the earliest data points. No data are available for Lithuania and Ireland before 2002 and 2006 respectively. In the case of Germany, before 2005, there was a problem with the variables used to link household members between them. Therefore, we decided to use Germany 2005.

Sweden, Norway, Denmark, Switzerland do not provide HH level information like HHLINK, HHSPOU, HHFATH, HHMOTH. Therefore, they are excluded from the analysis.

3. DEMOGRAPHIC AND HEALTH SURVEYS (DHS)
Because of their particular design (only individuals under 15 years old have a variable indicating if the parents live in the household), we have not included DHS samples in the analysis of individual variables.
