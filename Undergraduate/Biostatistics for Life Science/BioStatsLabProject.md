`					`Primary Biliary Cirrhosis

**Introduction**:

Primary biliary cholangitis (PBC), formerly termed primary biliary cirrhosis, is an autoimmune disorder that leads to the gradual destruction of intrahepatic bile ducts, resulting in periportal inflammation and cholestasis. (Siddiqui & Ahmad, 2023) Patients with primary biliary cholangitis can have other autoimmune conditions such as Hashimoto thyroiditis, CREST syndrome, Sjogren syndrome, rheumatoid arthritis, telangiectasias, and celiac disease (Pandit & Samant, 2023). Primary biliary cholangitis is common among women of middle age worldwide. The disease ratio among females to males is 9:1. The diagnosis is usually made in women aged between 30 and 60(Pandit & Samant, 2023). Liver transplant is the standard gold treatment for primary biliary cholangitis, patients are extremely likely to develop chronic cirrhosis, intractable pruritus, and severely deranged bilirubin levels. These patients should be evaluated for a liver transplant. (Pandit & Samant, 2023).

Primary biliary cirrhosis is characterized immunologically by the presence of high-titre autoantibodies directed at pyruvate dehydrogenase complex (PDC) (Khanna et al., 2018). The anti-mitochondrial antibody is found in 85% of the cases. The anti-mitochondrial antibody binds to lipoic acid containing the E2 component of the pyruvate dehydrogenase complex that is located on the mitochondrial inner membrane (Pandit & Samant, 2023). The activated macrophages, together with activated T cells and anti-mitochondrial antibodies (AMAs), produce a proinflammatory response with subsequent damage to BECs resulting in biliary inflammation and portal fibrosis (Bossen et al., 2023).

A study published in the Efficacy and Mechanism Evaluation journal for the UK National Institute of Health Research on randomized controlled trial show Ursodexycholic as the principal drug used in Bile acid therapy for PBC (Khanna et al., 2018).It is effective at slowing or stopping progression to the end stage in the majority of patients, probably through a reduction of secondary liver injury as a result of retention of cytotoxic hydrophobic bile acids occurring in the context of bile duct loss. (Khanna et al., 2018) Up to 40% of PBC patients are under-responsive to UDCA and are at increased risk of progression of the disease to cirrhosis (Khanna et al., 2018).

` 	`A new drug to treat PBC is under active investigation. Because of this, we will analyze data from the Mayo Clinic trial in PBC of the liver conducted between 1974 and 1984. A total of 424 PBC patients, referred to Mayo Clinic during that ten-year interval, met eligibility criteria for the randomized placebo-controlled trial of the drug D-penicillamine (Therneau et al, 2000).











Materials and Methods:

`	`The data for the Primary Biliary Cirrhosis study was analyzed through the open-source programming language R. We installed and used the tidyverse, codetools, report and sjlabelled.



First, we made histograms and dot plots for each variable in the data to visualize it and look for outliers in distribution. Then, we performed a Shapiro-wilk test to test for normality. Depending on the result of this test, we will determine what statistical tests to use when comparing rows of variables with a data column. For normally distributed data we employed statistical variation methods to calculate the standard deviation, variance and mean of data rows. For data that did not follow a gaussian distribution (normal), we computed the median and interquartile range using non-parametric methods.

The next stage in the study was to create a variable to sort patient status, which was termed "Patient\_Is" in order to examine the differences between deceased and non-deceased patients.  The Shapiro-Wilk test was used to identify the analysis for the variable "deceased".  Where the Shapiro-Wilk test is significant (p <0.05), we rejected the null hypothesis and performed a Wilcoxon test can be employed. If the data is not significant (p>0.05), a t-test is executed. The t-test was then used to examine the results of both variables. If the t-test was significant (p <0.05), the outcome was not random. If the p value is more than 0.05, we fail to reject the null hypothesis, and determine that the difference in means is due to chance. Given this case, we performed a Fisher exact test was used to comprehend or examine the independence of the dependent groups.

Then, performed a Shapiro-wilk test to measure any differences across all variables when patients with Primary Biliary Cirrhosis take D-penicillamine against patients who were given a placebo with a shapiro test for normality. With the value of this normality test we will decide on using a Welch Two Sample t-test (shapiro result =  p>0.05) or a Wilcox-test (shapiro result = p<0.05)  for normally and non-normally distributed data respectively. Once analysis of the t-test was taken, a Fisher exact test or Chi-squared independence test was conducted for the two variables “deceased and non-deceased”.  Based on the p-values, the test can tell us the independence (P>0.05) or dependence (P<0.05) of the variables.

`	`A Shapiro-wilk test was performed to look at the differences between patients with Primary Biliary Cirrhosis who have taken D-Penicillamine versus patients who took the placebo and their survival rate. If the shapiro-wilk test was significant (P<0.05) then the interquartile range or Wilcoxon sum test was performed. If the Wilcoxon sum test was significant then there was a statistical difference between the two variables: D-Penicillamine and placebo. If the Shapiro-wilk test was not significant then a t-test was performed to detect if the statistical difference was due to chance and the mean and standard deviation were computed.  Once analysis of the t-test was taken, a Fisher exact test or Chi-squared independence test was conducted for the two variables “deceased and non-deceased”.  A different variable called “deceased” was computed for numerical tests, so based on the p-values, the test can tell us the independence (P>0.05) or dependence (P<0.05) of the variables.


`	`The final stage was to run an ANOVA test to see if there were any differences in the means of blood bilirubin levels and triglycerides, albumin levels and triglycerides, and alkaline phosphatase levels and triglycerides. If the ANOVA test revealed a significant difference (p<0.05), a paired t test was used to determine significance in the t-test. Following the t-test, a new variable for triglyceride. We created a variable for triglyceride levels that ranks: less or equal than 150 as “Normal”, greater or equal than 150 as “Mildly High”, greater than 200 as “High” and greater or equal than 500 as “Very High”. Lastly, we compared mortality variation based off administrated drug,


Results:

The Mayo\_Hepato dataset provides information on 424 PBC patients gathered over a period of ten years (FNA) and a score of different measurements/characteristics taken from histological preparations.

` `For each characteristic variable we studied the difference between the central dispersion and dispersion measurements of row variables like serum bilirubin, urine copper level, cholesterol, and more between the status of subject. All characteristic variables exhibit a statistically significant difference depending on the ‘Deceased’ and ‘Not Deceased’ criteria except for “Presence of Blood Vessel Malformations in the Skin.”

This can be seen in Table 1: Age(p = 0.002024), Sex [females] ( p = 0.002593), albumin levels ( p = 0.0006871), alkaline phosphatase levels (p = p<2.2e-16), aspartate aminotransferase ( p = p<2.2e-16), serum bilirubin ( p = p<2.2e-16), serum cholesterol ( p = p<2.2e-16), triglyceride levels ( p = p<2.2e-16), urine copper levels ( p = p<2.2e-16), platelet count (p= p<2.2e-16), edema ( p = 0.0007948), presence of hepatomegaly or enlarger liver ( p = 1.631e-14) and histologic state of disease (p = 3.159e-05). Subsequently, we input measurements of central tendency depending on normality of data variables. If the data is normally distributed we plugged mean and standard deviation. However, if it is not normally distributed we use median and interquartile range, these values can be seen in table 2.

Only triglycerides, ascites, hepatomegaly, sex, and blood vessel malformation show statistically significant differences between the type of treatment received while the other variables show no significant differences. This is shown in Table 3: time over treatment (p = 0.204), age over treatment (p = 0.1117), status over treatment (p = 0.5146), albumin levels over treatment (p = 0.4394), alkaline phosphatase levels over treatment (p =0.2276), aspartate aminotransferase over treatment (p = 0.1087),  serum bilirubin levels over treatment ( p = 0.6345), serum cholesterol levels (p = 0.3678), triglycerides (mg/dL) over treatment ( p =**0.02152**) copper in urine over treatment (p= 0.2608), platelet count over treatment (p = 0.3953), status over treatment (p = 0.2913), edema over treatment (p=0.2392), ascites presence over treatment ( p = **0.001904**), presence of hepatomegaly or enlarged liver over treatment (p-value < **2.2e-16**), presence of blood vessel malformation over treatment (  p-value < **2.2e-16** ), sex over treatment ( p = **0.00604**)

Table 1: Descriptive statistics depending on the Deceased/Not Deceased status

||<a name="_hlk132867330"></a>Deceased (N=60)                 |Not Deceased(N=88)|p-Value|
| :- | :- | :- | :- |
|Age|53\.3|47\.8|<p>t-test:</p><p>0\.002024</p>|
|Sex (Females)|50|85|Chi-squared test: 0.002593|
|Albumin Levels (unit)|3\.35|3\.61|<p>t.test:</p><p>0\.0006871</p>|
|Alkaline Phosphatase Levels|2654|1508|<p>Wilcoxon test:</p><p>p<2.2e-16</p>|
|Aspartate Aminotransferase Levels|142|108|<p>Wilcoxon test:</p><p>P<2.2e-16</p>|
|Serum Bilirubin Levels|5\.03|1\.75|<p>Wilcoxon test:</p><p>P<2.2e-16</p>|
|Serum Cholesterol Levels|392|336|<p>Wilcoxon test:</p><p>P<2.2e-16</p>|
|Triglycerides Levels|145|111|<p>Wilcoxon test:</p><p>P<2.2e-16</p>|
|Urine Copper Levels|127|57\.3|<p>Wilcoxon test:</p><p>P<2.2e-16</p>|
|Platelet Count|246|273|<p>Wilcoxon test:</p><p>P<2.2e-16</p>|
|Edema	|60|88|<p>Fisher's Exact Test for Count Data:</p><p>0\.0007948</p>|
|`    `No Edema (%(n))|76\.67%(n=46)|92\.04%(n=81)||
|`    `Untreated or Successfully Treated (%(n))|10% (n=6)|7\.96% (n=7)||
|`    `Edema Despite Diuretic Therapy (%(n))|13\.33%(n=8)|0||
|Presence of Hepatomegaly (%(n))|72\.9%(n=35)|35\.3%(n=24)|<p>Wilcoxon test:</p><p>1\.631e-14</p>|
|Presence of Ascites|10|0|<p>Wilcoxon test:</p><p>0\.001904</p>|
|Presence of Blood Vessel Malformations in the Skin |19|12 |<p>Chi-Square: </p><p>0\.2087</p>|
|Histologic Stage of Disease|59|88|<p>Fischer test:</p><p>3\.159e-05</p>|
|`    `Stage 1|0|5||
|`    `Stage 2|10|29||
|`    `Stage 3|16|37||
|`    `Stage 4|33|17||



` `Table 2: Descriptive Statistics of Quantifiable Variable (non-count variables)

||Mean|Standard Deviation|Median|Interquartile range (IQR)|
| :- | :- | :- | :- | :- |
|Age [Normally Distributed]|49\.878|10\.1478|||
|Sex |||||
|Albumin Levels (unit)|3\.503|0\.431558|||
|Alkaline Phosphatase Levels|||1391|866\.75|
|Aspartate Aminotransferase Levels|||109\.28|68\.5875|
|Serum Bilirubin Levels|||1\.30|2\.1|
|Serum Cholesterol Levels|||320\.5|138|
|Triglycerides Levels|||107\.0|60\.75|
|Urine Copper Levels|||63|63|
|Platelet Count|||256|128\.5|
|Edema	|||0|0|
|`    `No Edema (%(n))|||||
|`    `Untreated or Success|||||
|`    `Edema Despite Diuretic Therapy |||||
|Presence of Hepatomegaly |||1|1|
|Presence of Ascites|||1|1|
|Presence of Blood Vessel Malformations in the Skin |||0|NA|
|Histologic Stage of Disease|||1||
|`    `Stage 1|||||
|`    `Stage 2|||||
|`    `Stage 3|||||
|`    `Stage 4|||||

Table 3: Testing effect of placebo vs D-penicillamine treatment against all data columns.

|Column Variable|Test Performed|p-value|
| :- | :- | :- |
|Time until death|Welch Two Sample t-test|0\.204|
|Age|Welch Two Sample t-test|0\.1117|
|Sex|Fisher's Exact Test for Count Data|0\.00604|
|Status|Wilcoxon rank sum test with continuity correction|0\.5146|
|Albumin Levels (unit)|Wilcoxon rank sum test with continuity correction|0\.4394|
|Alkaline Phosphatase Levels|Wilcoxon rank sum test with continuity correction|0\.2276|
|Aspartate Aminotransferase Levels|Wilcoxon rank sum test with continuity correction|0\.1087|
|Serum Bilirubin Levels|Wilcoxon rank sum test with continuity correction|0\.6345|
|Serum Cholesterol Levels|Wilcoxon rank sum test with continuity correction|0\.3678|
|Triglycerides Levels|Wilcoxon rank sum test with continuity correction|**0.02152**|
|Urine Copper Levels|Wilcoxon rank sum test with continuity correction|0\.2608|
|Platelet Count|Wilcoxon rank sum test with continuity correction|0\.3953|
|Edema|Fisher's Exact Test for Count Data|0\.2392|
|Ascites|Wilcoxon signed rank test with continuity correction|**0.001904**|
|Presence of Hepatomegaly |Wilcoxon rank sum test with continuity correction|**<2.2e-16**|
|Presence of Blood Vessel Malformations in the Skin|Wilcoxon rank sum test with continuity correction|**<2.2e-16**|
|Histologic Stage of Disease|`	`Fisher's Exact Test for Count Data|0\.2913|

![Text

Description automatically generated](Aspose.Words.7d5eb5a0-2e6b-4bf8-a29a-20fc8b31e32b.001.png)ANOVA: Comparing triglyceride levels to bilirubin:

We created a variable for triglyceride levels that ranks: less or equal than 150 as “Normal”, greater or equal than 150 as “Mildly High”, greated than 200 as “High”  and greater or equal than 500 as “Very High”

Since the p-value is less than 0.05, we reject the null hypothesis. Therefore, any difference between albumin relative to triglyceride levels is entirely due to chance, no correlation.

![Text

Description automatically generated](Aspose.Words.7d5eb5a0-2e6b-4bf8-a29a-20fc8b31e32b.002.png)ANOVA: Comparing triglyceride levels to albumin:


We created a variable for triglyceride levels that ranks: less or equal than 150 as “Normal”, greater or equal than 150 as “Mildly High”, greated than 200 as “High”  and greater or equal than 500 as “Very High”

Since the p-value is greater than 0.05, we fail to reject the null hypothesis. Therefore, any difference between

ANOVA: Comparing triglyceride levels to alkaline phosphatase:

We created a variable for triglyceride levels that ranks: less or equal than 150 as “Normal”, greater or equal than 150 as “Mildly High”, greated than 200 as “High”  and greater or equal than 500 as “Very High”![Text

Description automatically generated](Aspose.Words.7d5eb5a0-2e6b-4bf8-a29a-20fc8b31e32b.003.png)

Since the p-value is greater than 0.05, we fail to reject the null hypothesis. Therefore, any difference between alkaline phosphatase and triglyceride levels is entirely due to chance, no correlation.

**Discussion**:

Data obtained from this 10 year long trial was used to categorize the possible relationship between PBC mortality and row variables. After reviewing literature evidence between triglyceride count and bilirubin were found (Siddiqui & Ahmad, 2023.)

In biliary atresia, the direct and indirect bilirubin levels are both elevated with the conjugated portion being more elevated. (Siddiqui & Ahmad, 2023.) There is an statistically significant ANOVA (p>0.01) correlation between the level of triglycerides and bilirubin and a correlation between the usage of D-penicillamine and triglyceride levels. This hints at a direct relationship between triglyceride and bilirubin. A multiple logistic regression would give us a more accurate idea on the relationship between triglyceride levels, bilirubin and ascites. Additionally, use of D-penicillamine had a significant effect on mortality respect of ascites (p = 0.001904), hepatomegaly(p **<**2.2e-16**)** and presence of blood vessel malformations on the skin(p = <2.2e-16 ) ) and triglycerides ( p = 0.02152), regression tests would give further evidence towards examination of the impact of these variables.








Literature Cited

Gong, Y., Frederiksen, S. L., & Gluud, C. (2004). D-penicillamine for primary biliary cirrhosis. *The Cochrane Database of Systematic Reviews*, *2004*(4), CD004789. <https://doi.org/10.1002/14651858.CD004789.pub2>

Siddiqui, A. I., & Ahmad, T. (2023). Biliary Atresia. In *StatPearls*. StatPearls Publishing. <http://www.ncbi.nlm.nih.gov/books/NBK537262/>

Pandit, S., & Samant, H. (2023). Primary Biliary Cholangitis. In *StatPearls*. StatPearls Publishing. <http://www.ncbi.nlm.nih.gov/books/NBK459209/>

Khanna, A., Jopson, L., Howel, D., Bryant, A., Blamire, A., Newton, J. L., Wilkinson, J., Steel, A. J., Bainbridge, J., Stefanetti, R., Cassidy, S., Houghton, D., & Jones, D. E. (2018). *Rituximab for the treatment of fatigue in primary biliary cholangitis (formerly primary biliary cirrhosis): A randomised controlled trial*. NIHR Journals Library. <http://www.ncbi.nlm.nih.gov/books/NBK499478/>

Bossen, L., Lau, T. S., Nielsen, M. B., Nielsen, M. C., Andersen, A. H., Ott, P., Becker, S., Glerup, H., Svenningsen, L., Eivindson, M., Kornerup, L., Kjeldsen, N. B., Neumann, A., Møller, H. J., Jepsen, P., & Grønbæk, H. (2023). The association between soluble CD163, disease severity, and ursodiol treatment in patients with primary biliary cholangitis. *Hepatology Communications*, *7*(4), e0068. <https://doi.org/10.1097/HC9.0000000000000068>

T Therneau and P Grambsch (2000), *Modeling Survival Data: Extending the Cox Model*, Springer-Verlag,

New York. ISBN: 0-387-98784-3.

