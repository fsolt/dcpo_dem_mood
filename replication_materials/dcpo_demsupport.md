---
output: 
  bookdown::pdf_document2:
    fig_caption: yes
    keep_md: yes
    toc: no
    number_sections: yes
    latex_engine: xelatex
    pandoc_args: --lua-filter=multiple-bibliographies.lua
  
title: |
  | Democracy, Public Support, 
  | and Measurement Uncertainty
  
  
# abstract: "Do democratic regimes depend on public support to avoid backsliding? Does public support, in turn, respond thermostatically to changes in democracy? Two prominent recent studies (Claassen 2020a, 2020b) reinvigorated the classic hypothesis on the positive relationship between public support for democracy and regime survival---and challenged its reciprocal counterpart---by using a latent variable approach to measure mass democratic support from cross-national survey data. Both studies, however, used only the point estimates of democratic support; we show that incorporating the concomitant measurement uncertainty into these analyses reveals that there is no support for either study's conclusion. Efforts to minimize the uncertainty by incorporating additional survey data still fail to yield evidence in support of either hypothesis. These results underscore the need both for more nuanced analyses of the relationships between public support and democracy and for taking measurement uncertainty into account when working with latent variables. [147/150 words]"
# keywords: "Democratization, public support, regime survival, measurement uncertainty, latent variables."

date: " "
editor_options: 
  markdown: 
    wrap: sentence
tables: true # enable longtable and booktabs
citation_package: natbib
citeproc: false
fontsize: 12pt
indent: true
linestretch: 1.5 # double spacing using linestretch 1.5
bibliography_text: 
  - dcpo_demsupport_text.bib
bibliography_app: 
  - dcpo_demsupport_app.bib
biblio-style: apsr
citecolor: black
linkcolor: black
endnote: no
header-includes:
      - \usepackage{array}
      - \usepackage{caption}
      - \usepackage{graphicx}
      - \usepackage{siunitx}
      - \usepackage{colortbl}
      - \usepackage{multirow}
      - \usepackage{hhline}
      - \usepackage{calc}
      - \usepackage{tabularx}
      - \usepackage{threeparttable}
      - \usepackage{wrapfig}
      - \usepackage{fullpage}
      - \usepackage{pdflscape} #\usepackage{lscape} better for printing, page displayed vertically, content in landscape mode, \usepackage{pdflscape} better for screen, page displayed horizontally, content in landscape mode
      - \newcommand{\blandscape}{\begin{landscape}}
      - \newcommand{\elandscape}{\end{landscape}}
      - \usepackage{titlesec}
      - \titleformat*{\section}{\normalsize\bfseries}
      - \titleformat*{\subsection}{\normalsize\itshape}
---

# Authors {-}

- Yuehong 'Cassandra' Tai, ORCID: https://orcid.org/0000-0001-7303-7443, Ph.D. Candidate, Department of Political Science, University of Iowa, yuehong-tai@uiowa.edu.
- Yue Hu, corresponding author, ORCID: https://orcid.org/0000-0002-2829-3971, Associate Professor, Department of Political Science, Tsinghua University, yuehu@tsinghua.edu.cn
- Frederick Solt, ORCID: https://orcid.org/0000-0002-3154-6132, Associate Professor, Department of Political Science, University of Iowa, frederick-solt@uiowa.edu.

# Acknowledgement {-}

The three authors contributed equally to this work. 
We thank the editors and anonymous reviewers for their helpful comments.
All errors remain our own.
Replication files are available at the American Political
Science Review Dataverse: https://doi.org/10.7910/DVN/YM67QE.

# Funding {-}
Yue Hu acknowledges the support from the National Natural Science Foundation of China (Grant No. 72004109).

\pagebreak

# Abstract {-}

Do democratic regimes depend on public support to avoid backsliding? Does public support, in turn, respond thermostatically to changes in democracy? Two prominent recent studies (Claassen 2020a, 2020b) reinvigorated the classic hypothesis on the positive relationship between public support for democracy and regime survival---and challenged its reciprocal counterpart---by using a latent variable approach to measure mass democratic support from cross-national survey data. Both studies, however, used only the point estimates of democratic support; we show that incorporating the concomitant measurement uncertainty into these analyses reveals that there is no support for either study's conclusion. Efforts to minimize the uncertainty by incorporating additional survey data still fail to yield evidence in support of either hypothesis. These results underscore the need both for more nuanced analyses of the relationships between public support and democracy and for taking measurement uncertainty into account when working with latent variables.

\pagebreak



It has long been argued that democratic regimes and public support for them are mutually reinforcing: that high levels of public support ensure democracies remain strong, and that experience with democratic governance generates robust public support [see, e.g., @Lipset1959a; @Easton1965].
But the evidence for either part of this claim has been decidedly mixed.
Countries with greater democratic support have been found to become stronger and more stable democracies [e.g., @InglehartWelzel2005a, pp. 251-254] and just the opposite [@FailsPierce2010, pp.182-183].
Similarly, studies have alternately found that more experience with democracy yields more democratic support [e.g., @FailsPierce2010, p. 183] or instead that long-established democracies are suffering from democratic fatigue [e.g., @FoaMounk2017].

One important reason for these mixed results is the difficulty in measuring democratic support over time and across many countries.
Public support for democracy cannot be directly observed, and its incorrect measurement will limit inferences about the relationships between public opinion and institutional development.
Further, the survey data available across countries and over time on support for democracy---or indeed most topics in public opinion---are sparse and incomparable, greatly hindering broadly comparative research.
Recent pioneering studies have sought to overcome the hurdle of sparse and incomparable data by developing latent variable measurement models of public opinion [see @CaugheyEtAl2019; @Claassen2019; @Solt2020a].
A pair of prominent recent works took advantage of this latent variable approach to measure democratic support for over one hundred countries for up to nearly three decades and assess, respectively, its consequences for and roots in democratic change [@Claassen2020a; @Claassen2020].
The first of these works concluded, supporting the classic argument, that mass support had a positive impact on democratic change, especially the endurance of democracy [@Claassen2020a, pp. 127-130]. The second directly contradicted the classic argument, concluding that democratic change has a thermostatic effect on public support, that is, that rather than generating its own support, deepening democracy provokes a backlash and it is instead democratic backsliding that calls forth greater public support [@Claassen2020, pp. 46-50].

The models employed in these studies' analyses, though, do not account for uncertainty in their measurement of democratic support.
Because they are unobserved, latent variables are inherently accompanied by measurement uncertainty.
To leave this uncertainty unacknowledged is to make the implausible assumption that the latent variables are measured perfectly, an assumption which distorts both statistical and substantive inference [see, e.g., @CrabtreeFariss2015].

Here, we reexamine the classic arguments about support for democracy and democratic change tested in these two pieces while correcting this oversight.
In addition to incorporating the measurement uncertainty, we also sought to reduce it by expanding considerably the survey data drawn upon and re-estimating democratic support for 144 countries for up to 33 years between 1988 and 2020.
Our analyses reveal that the significant relationships between public support and democratic change disappear once measurement uncertainty is taken into account, both in replications with the studies' original data and in our extension analyses that incorporate additional data.
That is, simply taking into account measurement uncertainty---making no change to the specification of the models---reveals there is no empirical support for either claim put forward in these two works: declining democratic support does not signal subsequent democratic backsliding, and changes in democracy do not spur a thermostatic response in democratic support.
  
There are several important implications of these null results.
They point to a need for closer attention to the conditional aspects of the classic theory [see @Lipset1959a, pp. 86-89; @Easton1965, pp. 119-20].
On the one side, the effect of democracy on public support may depend not on its mere existence but on its effectiveness [see @Magalhaes2014] and particularly with regard to redistribution [see @KrieckhausEtAl2014].
On the other, the impact of public support on democracy may depend on the extent to which those who support democracy are also dissatisfied with the current regime's performance [see @QiShin2011].
Similarly, the results presented here are further evidence that the survey items commonly employed to measure democratic support are inadequate to the task.
Because these questions contain no information on respondents' support for democracy relative to other values with which it may come into conflict [see, e.g., @GrahamSvolik2020; @SimonovitsEtAl2022] or on whether respondents even understand the meaning of the democracy they are claiming to support [see, e.g., @KirschWelzel2019; @WuttkeEtAl2020], these questions appear to miss capturing the true extent of the support among the public that democracy will actually find when public support is in fact most needed.
Our results also reinforce arguments that relationships between democracy and public support unfold only over the long term [see, e.g., @WelzelEtAl2017] and that democratic change in the short term is instead best understood as an elite-driven phenomenon [see, e.g., @HaggardKaufman2021].

We draw two conclusions, one methodological and one substantive.
Many constructs in social science---from democracy to corruption to public opinion---are latent variables, and recent advances have made estimating them much more practicable.
As latent variable measurement models become more commonly used, it is absolutely necessary for researchers employing them to incorporate the associated uncertainty into their analyses.
As demonstrated here, this can be done straightforwardly using the method of composition [see @Tanner1993, p.52; @TreierJackman2008, p.215] without requiring any further change in model specification. 

And, at a time when democracy is seen as under threat around the world [e.g., @Diamond2015], taken together, Claassen [-@Claassen2020a; -@Claassen2020] send what is ultimately a reassuring message: the fate of democracy rests with us, the public, and when democratic institutions are undermined, we will swing to their support and constitute "an obstacle to democratic backsliding" [@Claassen2020, p. 51].
Both of these assertions may well be true, but the evidence we have, properly assessed, does not support them.
There is no room for complacency.




# Method {-}

We proceed in three steps.
First, we reproduce the original analyses of Claassen [-@Claassen2020a; -@Claassen2020], which included only the point estimates of the latent variable of democratic support and so exclude its measurement uncertainty.
Second, we collect the original cross-national survey data, replicate the latent variable measure of democratic support used in the two articles, and conduct the articles' analyses again, this time maintaining the entire distribution of estimates of democratic support in each country-year.^[
Additional details on this data replication process are found in the Online Supplementary Materials (OSM) \@ref(dgp).]
As democracy is also a latent variable in these analyses, we include the quantified uncertainty in its estimates as well, along with that for corruption in the models of @Claassen2020.^[
We also include the uncertainty in GDP per capita and resource dependence due to missing data by using multiple imputation rather than the single imputation employed in the original analyses.]
In the third step, we collect even more survey data---increasing these source data by one-third---and re-estimate the two articles' analyses once more, again maintaining the full distribution of estimates to preserve measurement uncertainty.^[
We attempted to make fuller use of the available survey data by employing the DCPO model [@Solt2020a], which unlike the @Claassen2019 model does not dichotomize ordinal responses.  The better fit of the DCPO model to the data on democratic support [see @Solt2020a, 10-12] and the additional information it incorporates, however, did not yield substantively different results; see OSM \@ref(dcpo).]

## Incorporating Uncertainty {-}

Although measurement uncertainty has not yet attracted attention in the field of comparative public opinion, latent variables are always estimated with a quantifiable amount of measurement error, and ignoring measurement error in analyses can attenuate, exaggerate, or even reverse coefficient estimates as well as bias standard errors [see, e.g., @BoundEtAl2001, p. 3709; @CaugheyWarshaw2018a, p. 254].
In light of this, recent studies measuring other latent variables have recommended incorporating their measurement uncertainty in analyses [see @SolisWaggoner2021, p. 18; @GandhiSumner2020, p. 1553], and research examining the consequences of public opinion in the United States has done so [see, e.g., @KastellecEtAl2015, pp. 791-792; @CaugheyWarshaw2018a, p. 254]. 
Therefore, after replicating the original analyses that use only the point estimates for public support and the other variables included in the model, we perform the analyses again only this time incorporating uncertainty in the articles' models using our replicated data.
We conduct inferences from the distributive data via the technique known as the "method of composition"  [MOC, @Tanner1993, p. 52].
MOC accounts for uncertainty from opinion estimates and analysis models through each modeling stage.
In the analysis stage, uncertainty is incorporated through simulation based on the variance-covariance matrix of a model without requiring any change to its specification.
This allows MOC to be broadly applicable to many different types of models, including time-series-cross-sectional models [@CaugheyWarshaw2018a, p. A15-16], Cox proportional hazards models [@TreierJackman2008, p. 215], or models of individual-level roll call voting [@KastellecEtAl2015, p. 791].^[For additional details on the MOC technique, see  OSM \@ref(moc).]




## Adding More Data {-}

To provide a further test of the classic arguments on democracy and public support, we generated estimates of democratic support using the same procedure as in Claassen [-@Claassen2020a; -@Claassen2020] on a bigger data set, assembling as much survey data on democratic support as possible.
We employed 4,905 national opinions on democracy from 1,889 national surveys, representing a 32.0% and 37.3% increase respectively over the 3,716 opinions and 1,376 national surveys used in Claassen [-@Claassen2020a; -@Claassen2020].^[
These figures represent the survey data actually used in estimating public support for democracy; as in Claassen [-@Claassen2020a; -@Claassen2020], countries for which two separate years of survey data were not available were excluded.]

# Results {-}

Figure \@ref(fig:plot-fullAJPS) presents the reanalyses of the hypothesis that public support influences the level of democracy [@Claassen2020a, Table 1].
The lighter, lefthand set of results replicate the analysis of @Claassen2020a, including its exclusion of measurement uncertainty by using only the point estimates of public democratic support and the other variables measured with quantified error (i.e., democracy and corruption), and they reproduce that article's findings.
The middle results introduce a single change: the uncertainty in the measurement of public support and these other variables is taken into account.
In all four models, the positive coefficients for democratic support are no longer statistically significant.
The darker, righthand results also incorporate uncertainty but additionally replace the estimates of democratic support with those based on our expanded dataset; this change works to increase the number of observations analyzed as well.
Although the confidence intervals shrink considerably, the coefficient estimates move much closer to zero: the hypothesis remains unsupported.













\begin{figure}

{\centering \includegraphics{dcpo_demsupport_files/figure-latex/plot-fullAJPS-1} 

}

\caption{The Effect of Public Support on Democracy with Uncertainty}(\#fig:plot-fullAJPS)
\end{figure}

In Figure \@ref(fig:plot-fullAPSR), we examine the thermostatic model of democratic support per @Claassen2020 [p. 47, Table 1, 49, Table 2].
The negative coefficient estimates for change in liberal democracy in the lefthand set of results, which do not take uncertainty into account, imply that the immediate effect of a increase in the level of democracy is a decline in public support for democracy and of a decrease in democracy an expansion of support---that democratic support indeed does respond thermostatically to democracy. 
However, the middle results demonstrate that this thermostatic effect, too, does not  hold after the measurement uncertainty is accounted for. 
And again, the righthand results reveal that the additional data of our extension do not provide support for the original conclusion.

In short, the conclusions of Claassen [-@Claassen2020a; -@Claassen2020] that democratic support has a positive effect on democracy and change in democracy a negative effect on change in support are not empirically supported once measurement uncertainty is taken into account, even when more data are used.  













![(\#fig:plot-fullAPSR)The Effect of Democracy on Change in Public Support with Uncertainty](dcpo_demsupport_files/figure-latex/plot-fullAPSR-1.pdf) 

# Discussion {-}

These null results have a number of important substantive implications.
First, they underscore that it is crucial to recognize the conditional aspects of the classic theory regarding democracy and democratic support.
With regard to how levels of democracy affect public support, even the early proponents of the classic argument did not contend that the mere existence of democratic institutions, no matter how consistently feckless and ineffective, would generate support among the public: instead, they maintained, public support would be gained through experience with government performance that was generally effective [@Lipset1959a, pp. 86-89; @Easton1965, pp. 119-120].
There is some empirical evidence for this, with government effectiveness positively related to public support among democracies and negatively related in non-democracies [@Magalhaes2014]. 
The finding of @KrieckhausEtAl2014 that income inequality is strongly negatively related to public support in democracies suggests that performance regarding redistribution is particularly important.
On the reverse part of the classic argument, @QiShin2011 suggests that democratic support alone cannot be expected to generate democratic change and oppose backsliding.
Instead, that work contends, it is the combination of democratic support and dissatisfaction with current regime performance that generates demand for greater democracy.
Whether these conditional relationships exist among the newly available latent-variable data on democracy and democratic support remain questions for future research. 

Further, these results recommend building recent and more refined conceptualizations of democratic support into our measures.
In other words, the survey items employed by Claassen [-@Claassen2020a; -@Claassen2020]---which ask respondents to assess the desirability or appropriateness of democracy, to compare democracy to some undemocratic alternative, or to assess one of these alternatives---although often used by researchers, may not capture every aspect of democratic support necessary for it to play its hypothesized roles in the classic theory.
One possibility is that only those who profess to prefer democracy to its alternatives _and also_ value freedom of expression, freedom of association, and pluralism of opinion will take appropriate action when democracy is threatened [see, e.g., @SchedlerSarsfield2007].
Another is that respondents' other values, such as their policy preferences or partisanship, may weigh more heavily than their support for democracy: there is growing evidence that, at least in the United States, there are many for whom these other considerations excuse substantial transgressions against democracy [see @GrahamSvolik2020; @SimonovitsEtAl2022].
Yet another is that the answers to the above items reflect actual support for democracy only when respondents also either hold a robust understanding of what liberal democracy means or anchor their support in emancipative values of universal freedoms.
If they do not, their positive responses to these items indicate support for autocracy instead [see, e.g., @KirschWelzel2019; @WuttkeEtAl2022].
Taking any or all of these into account requires considering the _combination_ of attitudes: even the inclusion of additional questions in a unidimensional public opinion model such as that provided by @Claassen2019 will not be sufficient [see, e.g., @WuttkeEtAl2020].

Finally, by failing to provide evidence for a short-term relationship between democratic support and democracy, these null findings can be seen to lend additional support to other theories of regime change.
There are compelling arguments that episodes of democratic transition [see, e.g., @ODonnellEtAl1986] and backsliding [see, e.g., @HaggardKaufman2021] are best understood as products of elite decision making.
With regard to the latter, critics have charged that these claims overlook the extent of demand for authoritarian rule among the public [see, e.g., @Norris2021].
The null results reported here, by finding that year-to-year changes in democratic support bear little relationship to changes in democracy, highlight that levels of public support appear to relate to regime only over the long run [see also @WelzelEtAl2017], leaving elite decisions as a powerful explanation for when and how short-term developments unfold.


# Conclusion {-}

Simply taking measurement uncertainty into account, while making no changes to model specification, rendered the conclusions of both articles examined here without empirical support, even when we added a considerable quantity of additional data.
Methodologically, these results point to the absolute necessity of incorporating measurement uncertainty into analyses that include latent variables.
As the use of latent variables grows more common, political scientists should be aware that these variables' concomitant measurement uncertainty cannot be neglected.
Recent cross-national time-series measures of, for example, policy ideology across Europe [developed in @CaugheyEtAl2019], immigration attitudes in Europe [presented in @ClaassenMcLaren2021], and public gender egalitarianism worldwide [introduced in @WooEtAl2022], make possible a host of previously infeasible analyses, but the resulting research cannot be considered robust if it does not also incorporate these measures' quantified uncertainty.

Our results also have several substantive implications.
They highlight theoretical arguments that maintain that levels of democratic support undergird democracy only over the long term and so lend indirect support to other explanations for short-run changes in regime.
They also draw attention to the conditional nature of the classic argument on democracy and democratic support as well as to challenges in measuring the concept of democratic support that remain unmet by existing time-series cross-sectional latent-variable models.
Most importantly, the sanguine assessment that readers may draw from Claassen [-@Claassen2020a; -@Claassen2020]---that the fate of democracies depend on public support, and when eroded, their publics will rally to them---is not supported by the current evidence.
Those who would defend democracy have no grounds to be complacent.

\pagebreak
# References  {-}

<div id = 'refs_text'></div>

\pagebreak

# Online Supplementary Materials {-}
# (APPENDIX) Appendix {-}

\setcounter{page}{1}
\renewcommand{\thepage}{A\arabic{page}}
\setcounter{figure}{0}
\renewcommand{\thefigure}{A.\arabic{figure}}
\setcounter{table}{0}
\renewcommand{\thetable}{A.\arabic{table}}

# Replication Notes {#dgp}

Because the Dataverse replication files for @Claassen2020a and @Claassen2020 included only the point estimates of their variables, replicating those articles' analyses while incorporating the quantified uncertainty in their latent variables was not possible using only those files.
Instead, it required re-collecting all of the data employed in those articles directly from their original sources, that is, in the terminology of @GrossmanPedahzur2021, it required a primary replication rather than a secondary one.
We describe this process for the latent variables here.

## Democratic Support {-}

First, for each item used in the two articles' measure of democratic support, we identified the original variable names and corresponding values in each survey dataset. 
We then used the `dcpo_setup` and `format_claassen` functions of the `DCPOtools` R package [@SoltEtAl2019] to automate the process of generating a dataset of survey marginals.
We encountered a number of miscodings in the original data that we corrected.
For example, the item "necesitamos un líder fuerte que no tenga que ser eligido" was coded as `strong_lapop_2`, although according to the AmericasBarometer codebooks, this question and that coded as `strong_lapop_1` employed the identical language:

> Hay gente que dice que necesitamos un líder fuerte que no tenga que ser elegido a través del voto. AUT1 Otros dicen que aunque las cosas no funcionen, la democracia electoral, o sea el voto popular, es siempre
lo mejor. ¿Qué piensa usted? [Leer alternativas]

We therefore coded both as `strong_lapop_1`.
Other examples include inconsistent coding regarding what constitutes a "democracy-supporting" response. 
Here we use the corrected data for the replications with uncertainty to ensure that issues of data quality did not distort the results presented.

Another coding problem involves "don't know" and other survey non-responses. 
In Claassen [-@Claassen2020a; -@Claassen2020] all respondents who did not respond to a survey item are assumed to not support democracy and an undemocratic response is singly imputed to them.
We found this rule was sometimes inconsistently applied, but in any event, single imputation has long been well understood to incorporate overly strong assumptions and understate measurement error [see, e.g., @Rubin1987].
Therefore, we instead used multiple imputation.^[
In rare cases, non-responses that were imputed undemocratic responses in Claassen [-@Claassen2020a; -@Claassen2020] are the result of split samples, where respondents were not even presented with the survey item in question.
We exclude these explicitly 'missing at random' responses from the sample.]
As the more familiar model-based multiple imputation technique is infeasible across thousands of national surveys, we proceeded as follows to generate four imputations.
A first imputation assumes that attitudes among those who did not respond to an item are actually similar to the attitudes of those who did; that is, that the data are missing at random and so non-responses are excluded from the sample.
The second imputation follows Claassen [-@Claassen2020a; -@Claassen2020] in coding non-responses as a lack of support and so sets a lower bound on the proportion of the population who support democracy as evidenced by a particular survey item.
The third imputation, conversely, establishes an upper bound on democratic support indicated by an item by coding all non-responses as _support_ for democracy.
The last imputation is theoretically derived: it assumes that non-response in democratic contexts reflects social desirability bias and so a lack of support for democracy, while non-response in non-democratic contexts reflects fear of reprisal and so democratic support.
These four sets of survey marginals were then used to generate estimates of democratic support in Stan using the `supdem.stan.mod5.stan` script from the @Claassen2020a Dataverse materials.
Finally, the draws from the resulting posterior distributions were combined as in a model-based multiple imputation routine [see, e.g., @Rubin1987]; together, the draws quantify the uncertainty in the estimates of democratic support.

Finally, for the GMM models, the original publication presented the observation number based on the trimmed data, i.e., "2435", while the tabulating function in the replication file (line 86) uses the full used-observation number, "4735." 
We follow the replication file in the tables of Appendix B below.


## Democracy {-}

The democracy variables in the two articles are drawn from Version 8 of the V-Dem Dataset [@CoppedgeEtAl2018].
This version of the dataset includes draws from the posterior distribution of the estimates to quantify their uncertainty.
In our extension (the rightmost replication of each model in our Figures 1 and 2), we are able to extend the time series beyond the last year of Version 8, 2017, so we use the updated Version 10 instead.
Version 10, however, does not include posterior draws of the estimates, but rather standard errors.
For the purposes of incorporating this uncertainty into our analyses, we assumed these errors were normally distributed around the point estimates.

## Corruption {-}

The corruption variable used in @Claassen2020 is the Corruption Perceptions Index, which provides point estimates and standard errors for the years from 2012 to 2018.
For country-years beyond that range, we conservatively estimated standard errors by first identifying the country's maximum relative standard error (standard error/point estimate) during 2012-2018 and then multiplying this quantity by the country-year's point estimate.
For the purposes of incorporating this uncertainty into our analyses, we again assumed a normal distribution around the point estimates.

\pagebreak

# Numeric Results for Figures \@ref(fig:plot-fullAJPS) and \@ref(fig:plot-fullAPSR) {#tables_not_plots}



\begin{table}[H]

\caption{(\#tab:num-pointAJPS)The Effect of Public Support on Democracy (Original Results)}
\centering
\fontsize{8}{10}\selectfont
\begin{tabular}[t]{lcccc}
\toprule
  & Pooled & Pooled-Regime & GMM & GMM-Regime\\
\midrule
(Intercept) & 0.647 & 0.765 &  & \\
 & (0.947) & (0.998) &  & \\
Democracy (t-1) & 1.141 & 1.142 & 1.091 & 1.095\\
 & (0.080) & (0.080) & (0.079) & (0.083)\\
Democracy (t-2) & -0.163 & -0.164 & -0.203 & -0.200\\
 & (0.080) & (0.079) & (0.051) & (0.050)\\
Support (t-1) & 0.267 &  & 0.881 & \\
 & (0.094) &  & (0.366) & \\
Support in Democracy &  & 0.318 &  & 0.810\\
 &  & (0.108) &  & (0.344)\\
Support in Autocracy &  & 0.090 &  & 0.917\\
 &  & (0.210) &  & (0.672)\\
Log GDP Per Capita (t-1) & 0.015 & -0.001 & 0.388 & 0.366\\
 & (0.123) & (0.130) & (0.174) & (0.186)\\
GDP Per Capita Growth (t-1) & 0.007 & 0.007 & -0.016 & -0.014\\
 & (0.017) & (0.017) & (0.020) & (0.021)\\
Regional Democracy (t-1) & 0.008 & 0.008 & 0.055 & 0.051\\
 & (0.005) & (0.004) & (0.028) & (0.030)\\
Percent Muslim (t-1) & -0.002 & -0.002 & -0.014 & -0.013\\
 & (0.003) & (0.003) & (0.009) & (0.009)\\
Resource Dependence (t-1) & -0.367 & -0.373 & -1.196 & -1.128\\
 & (0.244) & (0.242) & (0.683) & (0.694)\\
\midrule
N observations & 2435 & 2435 & 4735 & 4735\\
N countries & 135 & 135 & 135 & 135\\
N instruments &  &  & 122 & 124\\
\bottomrule
\end{tabular}
\end{table}



\begin{table}[H]

\caption{(\#tab:num-pureAJPS)The Effect of Public Support on Democracy (With Uncertainty)}
\centering
\fontsize{8}{10}\selectfont
\begin{tabular}[t]{lcccc}
\toprule
  & Pooled & Pooled-Regime & GMM & GMM-Regime\\
\midrule
Democracy (t-1) & 0.806 & 0.804 & 0.778 & 0.779\\
 & (0.067) & (0.066) & (0.044) & (0.045)\\
Democracy (t-2) & 0.143 & 0.145 & 0.127 & 0.126\\
 & (0.066) & (0.065) & (0.041) & (0.043)\\
Support (t-1) & 1.468 &  & 2.799 & \\
 & (0.821) &  & (1.817) & \\
Support in Democracy &  & 1.739 &  & 3.083\\
 &  & (0.887) &  & (1.890)\\
Support in Autocracy &  & 0.722 &  & 1.915\\
 &  & (1.434) &  & (2.586)\\
Log GDP Per Capita (t-1) & 0.643 & 0.584 & 0.225 & 0.218\\
 & (0.516) & (0.542) & (0.163) & (0.177)\\
GDP Per Capita Growth (t-1) & 0.031 & 0.032 & -0.008 & -0.002\\
 & (0.080) & (0.081) & (0.084) & (0.092)\\
Regional Democracy (t-1) & 0.018 & 0.018 & 0.050 & 0.050\\
 & (0.009) & (0.009) & (0.036) & (0.038)\\
Percent Muslim (t-1) & -0.018 & -0.019 & -0.033 & -0.035\\
 & (0.014) & (0.015) & (0.027) & (0.030)\\
Resource Dependence (t-1) & -3.381 & -3.389 & -4.004 & -4.104\\
 & (1.357) & (1.344) & (2.235) & (2.213)\\
\midrule
N observations & 2430 & 2430 & 4726 & 4726\\
N countries & 134 & 134 & 135 & 135\\
N instruments &  &  & 122 & 122\\
\bottomrule
\end{tabular}
\end{table}

\begin{table}[H]

\caption{(\#tab:num-expAJPS)The Effect of Public Support on Democracy (Uncertainty \& More Data)}
\centering
\fontsize{8}{10}\selectfont
\begin{tabular}[t]{lcccc}
\toprule
  & Pooled & Pooled-Regime & GMM & GMM-Regime\\
\midrule
Democracy (t-1) & 0.809 & 0.814 & 0.873 & 0.876\\
 & (0.072) & (0.071) & (0.043) & (0.040)\\
Democracy (t-2) & 0.140 & 0.136 & 0.182 & 0.183\\
 & (0.071) & (0.070) & (0.044) & (0.045)\\
Support (t-1) & -0.119 &  & -0.510 & \\
 & (0.392) &  & (0.476) & \\
Support in Democracy &  & -0.032 &  & -0.622\\
 &  & (0.442) &  & (0.583)\\
Support in Autocracy &  & -0.333 &  & -0.310\\
 &  & (0.578) &  & (0.709)\\
Log GDP Per Capita (t-1) & 0.270 & 0.263 & 0.121 & 0.119\\
 & (0.249) & (0.249) & (0.091) & (0.093)\\
GDP Per Capita Growth (t-1) & 0.012 & 0.011 & 0.022 & 0.022\\
 & (0.033) & (0.035) & (0.035) & (0.034)\\
Regional Democracy (t-1) & 0.014 & 0.014 & -0.086 & -0.088\\
 & (0.022) & (0.022) & (0.063) & (0.063)\\
Percent Muslim (t-1) & -0.005 & -0.005 & -0.005 & -0.005\\
 & (0.004) & (0.004) & (0.006) & (0.006)\\
Resource Dependence (t-1) & 0.350 & 0.328 & 0.749 & 0.791\\
 & (0.544) & (0.534) & (0.669) & (0.663)\\
\midrule
N observations & 2792 & 2792 & 5443 & 5443\\
N countries & 141 & 141 & 143 & 143\\
N instruments &  &  & 130 & 130\\
\bottomrule
\end{tabular}
\end{table}

\blandscape



\begin{table}[H]

\caption{(\#tab:num-pointAPSR)The Effect of Democracy on Change in Public Support (Original)}
\centering
\fontsize{7}{9}\selectfont
\begin{tabular}[t]{lcccccccc}
\toprule
  & ECM & ECM-Regime & FD & FD-Regime & ECM Corrup & ECM Corrup-Regime & FD Corrup & FD Corrup-Regime\\
\midrule
Democratic Mood (t-1) & 0.473 & 0.473 &  &  & 0.433 & 0.432 &  & \\
 & (0.026) & (0.025) &  &  & (0.028) & (0.028) &  & \\
Democratic Mood (t-2) & -0.487 & -0.487 &  &  & -0.451 & -0.450 &  & \\
 & (0.025) & (0.025) &  &  & (0.027) & (0.027) &  & \\
Liberal Democracy (Difference) & -0.058 &  & -0.076 &  & -0.067 &  & -0.082 & \\
 & (0.023) &  & (0.028) &  & (0.031) &  & (0.034) & \\
Liberal Democracy (t-1) & 0.007 &  &  &  & 0.002 &  &  & \\
 & (0.003) &  &  &  & (0.004) &  &  & \\
Electoral Democracy (Difference) &  & 0.014 &  & 0.011 &  & 0.028 &  & 0.021\\
 &  & (0.031) &  & (0.033) &  & (0.039) &  & (0.040)\\
Electoral Democracy (t-1) &  & 0.002 &  &  &  & 0.006 &  & \\
 &  & (0.006) &  &  &  & (0.006) &  \vphantom{1} & \\
Minoritarian Democracy (Difference) &  & -0.053 &  & -0.076 &  & -0.066 &  & -0.087\\
 &  & (0.022) &  & (0.025) &  & (0.029) &  & (0.029)\\
Minoritarian Democracy (t-1) &  & 0.003 &  &  &  & -0.004 &  & \\
 &  & (0.006) &  &  &  & (0.006) &  & \\
Log GDP Per Capita (Difference) & 0.063 & 0.062 &  &  & 0.037 & 0.034 &  & \\
 & (0.040) & (0.040) &  &  & (0.044) & (0.045) &  & \\
Log GDP per capita (Difference) &  &  & 0.108 & 0.102 &  &  & 0.089 & 0.082\\
 &  &  & (0.052) & (0.053) &  &  & (0.051) & (0.051)\\
Log GDP (t-1) & 0.003 & 0.004 &  &  & -0.003 & -0.003 &  & \\
 & (0.002) & (0.002) &  &  & (0.003) & (0.003) &  & \\
Corruption (Difference) &  &  &  &  & -0.008 & -0.007 & -0.022 & -0.021\\
 &  &  &  &  & (0.016) & (0.016) & (0.017) & (0.017)\\
Corruption (t-1) &  &  &  &  & -0.012 & -0.013 &  & \\
 &  &  &  &  & (0.004) & (0.004) &  & \\
\midrule
N observations & 2300 & 2300 & 2435 & 2435 & 1949 & 1949 & 2040 & 2040\\
N countries & 135 & 135 & 135 & 135 & 135 & 135 & 135 & 135\\
\bottomrule
\end{tabular}
\end{table}



\begin{table}[H]

\caption{(\#tab:num-pureAPSR)The Effect of Democracy on Change in Public Support (With Uncertainty)}
\centering
\fontsize{7}{9}\selectfont
\begin{tabular}[t]{lcccccccc}
\toprule
  & ECM & ECM-Regime & FD & FD-Regime & ECM Corrup & ECM Corrup-Regime & FD Corrup & FD Corrup-Regime\\
\midrule
Democratic Mood (t-1) & -0.024 & -0.024 &  &  & -0.031 & -0.030 &  & \\
 & (0.029) & (0.030) &  &  & (0.031) & (0.032) &  \vphantom{1} & \\
Democratic Mood (t-2) & 0.002 & 0.001 &  &  & 0.003 & 0.001 &  & \\
 & (0.029) & (0.030) &  &  & (0.031) & (0.032) &  & \\
Liberal Democracy (Difference) & -0.002 &  & 0.001 &  & -0.002 &  & 0.001 & \\
 & (0.007) &  & (0.011) &  & (0.008) &  & (0.011) & \\
Liberal Democracy (t-1) & -0.002 &  &  &  & -0.003 &  &  & \\
 & (0.006) &  &  &  & (0.007) &  &  & \\
Electoral Democracy (Difference) &  & -0.002 &  & 0.002 &  & -0.001 &  & 0.001\\
 &  & (0.007) &  & (0.010) &  & (0.007) &  & (0.010)\\
Electoral Democracy (t-1) &  & -0.001 &  &  &  & -0.002 &  & \\
 &  & (0.007) &  &  &  & (0.007) &  & \\
Minoritarian Democracy (Difference) &  & -0.002 &  & -0.001 &  & -0.002 &  & 0.000\\
 &  & (0.007) &  & (0.009) &  & (0.007) &  & (0.010)\\
Minoritarian Democracy (t-1) &  & -0.002 &  &  &  & -0.003 &  & \\
 &  & (0.006) &  &  &  & (0.007) &  & \\
Log GDP Per Capita (Difference) & -0.016 & -0.020 &  &  & -0.032 & -0.030 &  & \\
 & (0.081) & (0.084) &  &  & (0.089) & (0.085) &  & \\
Log GDP per capita (Difference) &  &  & 0.029 & 0.026 &  &  & 0.019 & 0.024\\
 &  &  & (0.102) & (0.103) &  &  & (0.102) & (0.107)\\
Log GDP (t-1) & 0.006 & 0.006 &  &  & -0.003 & -0.003 &  & \\
 & (0.005) & (0.005) &  &  & (0.006) & (0.006) &  & \\
Corruption (Difference) &  &  &  &  & -0.001 & -0.001 & 0.001 & 0.001\\
 &  &  &  &  & (0.020) & (0.020) & (0.032) & (0.031)\\
Corruption (t-1) &  &  &  &  & -0.012 & -0.012 &  & \\
 &  &  &  &  & (0.008) & (0.007) &  & \\
\midrule
N observations & 2296 & 2296 & 2430 & 2430 & 2076 & 2076 & 2167 & 2167\\
N countries & 134 & 134 & 134 & 134 & 134 & 134 & 134 & 134\\
\bottomrule
\end{tabular}
\end{table}

\begin{table}[H]

\caption{(\#tab:num-expAPSR)The Effect of Democracy on Change in Public Support (Uncertainty \& More Data)}
\centering
\fontsize{7}{9}\selectfont
\begin{tabular}[t]{lcccccccc}
\toprule
  & ECM & ECM-Regime & FD & FD-Regime & ECM Corrup & ECM Corrup-Regime & FD Corrup & FD Corrup-Regime\\
\midrule
Democratic Mood (t-1) & -0.030 & -0.030 &  &  & -0.042 & -0.043 &  & \\
 & (0.026) & (0.027) &  &  & (0.028) & (0.029) &  & \\
Democratic Mood (t-2) & 0.008 & 0.008 &  &  & 0.009 & 0.009 &  & \\
 & (0.026) & (0.028) &  &  & (0.028) & (0.028) &  & \\
Liberal Democracy (Difference) & 0.001 &  & 0.000 &  & 0.001 &  & 0.000 & \\
 & (0.006) &  & (0.009) &  & (0.006) &  & (0.010) & \\
Liberal Democracy (t-1) & 0.002 &  &  &  & 0.003 &  &  & \\
 & (0.004) &  &  &  & (0.004) &  &  & \\
Electoral Democracy (Difference) &  & 0.004 &  & 0.000 &  & 0.004 &  & 0.000\\
 &  & (0.006) &  & (0.010) &  & (0.007) &  & (0.010)\\
Electoral Democracy (t-1) &  & 0.006 &  &  &  & 0.006 &  & \\
 &  & (0.006) &  &  &  & (0.006) &  & \\
Minoritarian Democracy (Difference) &  & -0.002 &  & -0.001 &  & -0.002 &  & -0.001\\
 &  & (0.006) &  & (0.008) &  & (0.006) &  & (0.009)\\
Minoritarian Democracy (t-1) &  & -0.004 &  &  &  & -0.003 &  & \\
 &  & (0.006) &  &  &  & (0.007) &  & \\
Log GDP Per Capita (Difference) & -0.007 & -0.004 &  &  & -0.020 & -0.024 &  & \\
 & (0.073) & (0.072) &  &  & (0.076) & (0.075) &  & \\
Log GDP per capita (Difference) &  &  & -0.036 & -0.027 &  &  & -0.037 & -0.031\\
 &  &  & (0.095) & (0.093) &  &  & (0.098) & (0.098)\\
Log GDP (t-1) & 0.003 & 0.003 &  &  & -0.011 & -0.011 &  & \\
 & (0.004) & (0.004) &  &  & (0.005) & (0.005) &  & \\
Corruption (Difference) &  &  &  &  & -0.011 & -0.010 & 0.004 & 0.005\\
 &  &  &  &  & (0.019) & (0.018) & (0.029) & (0.029)\\
Corruption (t-1) &  &  &  &  & -0.022 & -0.023 &  & \\
 &  &  &  &  & (0.006) & (0.006) &  & \\
\midrule
N observations & 2655 & 2655 & 2794 & 2794 & 2386 & 2386 & 2478 & 2478\\
N countries & 141 & 141 & 141 & 141 & 139 & 139 & 139 & 139\\
\bottomrule
\end{tabular}
\end{table}

\elandscape


# The Method of Composition {#moc}

In our analysis models, we have latent variables in both sides of the equations: public democratic support, democracy, and corruption.
Since measurement uncertainty associated with these latent variables can propagate into the inferences over coefficient parameters in models, we incorporate uncertainty by employing the "Method of Composition" [@Tanner1993, p. 52], which has often been applied in analyses with latent variables in political science [see, e.g., @TreierJackman2008; @KastellecEtAl2015; @CaugheyWarshaw2018a].

As @CaugheyWarshaw2018a [p. A-15] explained, the main idea of MOC is to estimate the marginal distribution of coefficient parameter vector $\beta$, integrating over the uncertainty in latent variables $\theta$.
More explicitly, MOC integrates the joint density of $\beta$ and $\theta$ over the distribution of $\theta$.

\begin{equation}
p(\beta,\theta|w,y, Z) = p(\beta|\theta,w,y)p(\theta|Z).
\end{equation}

where $\theta$ is latent variables with measurement errors conditional on data Z and a measurement model, $w$ is other predictors without errors, $Z$ is indicators for latent variables $\theta$, and $y$ is the outcome variable.
In this way, we incorporate uncertainty in measuring predictor $\theta$, and uncertainty in the effects of latent variables $\theta$ and other variables $w$ on outcome variable $y$ [@TreierJackman2008, p. 215].

To sample from the conditional density and the marginal density in the right side of the equation, we follow the iterative Monte Carlo procedure described by @TreierJackman2008, at iteration $t$,

1. We sample $\theta^{t}$ from its posterior distribution $p(\theta|Z)$.

2. For each analysis model, we run the model with $\theta^{t}$, and $w$, and save the coefficient estimates $\hat{\beta^{t}}$ and variance-covariance matrix of $\hat{\beta^{t}}$, $\hat{V^{t}}$, both of which change due to the uncertainty in $\theta$.

3. We sample $\tilde{\beta^{t}}$ from the multivariate normal density with mean vector $\hat{\beta^{t}}$ and variance-covariance matrix $\hat{V^{t}}$.

In the step 3, the marginal distribution of a parameter vector $\beta$ was estimated, integrating over $p(\theta|Z)$:

\begin{equation}
p(\beta|w,y) = \int_{\theta} p(\beta|\theta,w,y)p(\theta|Z) \,d\theta
\end{equation}

In our re-analyses, we incorporate uncertainty for five variables, public support for democracy, liberal democracy, electoral democracy, the liberal component index, and the corruption perceptions index.
For each of these five latent variables, we take 900 draws from its posterior distribution. We duplicate the dataset of variables "without" measurement error 900 times, and assign them to each a different random draw from the distributions of variables with measurement error, which yields 900 datasets.
In the next step, we run each of analysis models with these 900 datasets independently and save the resulting estimates of coefficients and the matrix of variance-covariance for each run.
We then draw one sample from the multivariate normal distribution with the mean vector of coefficient estimates and variance-covariance matrix produced from each run.
This procedure finally yields 900 samples of estimated coefficients drawn from the joint density of $\beta$ and $\theta$.
We calculate point estimates and standard errors based on these 900 samples.

\pagebreak

# DCPO Replication {#dcpo}

To make fuller use of the available survey data, we also replicated the tests of the classic arguments on democracy and public support using the DCPO model put forward in @Solt2020a on our expanded data set.
The DCPO model has several advantages over the @Claassen2019 model used in Claassen [-@Claassen2020a; -@Claassen2020].
First, while the @Claassen2019 model dichotomizes responses and so discards some information provided by the 50 ordinal survey items (of 52 total) employed in Claassen [-@Claassen2020a; -@Claassen2020], the DCPO model makes use of all of the information available from these ordinal items [@Solt2020a, p.5].
Second, as the DCPO model includes both parameters for the dispersion of each survey item and for the standard deviation of aggregate public opinion in each country-year, it is a complete population-level item-response model and so, unlike the @Claassen2019 model, is explicitly derived from an individual-level model of survey responses [@Solt2020a, pp. 3-4; see also @McGann2014].
Third, to produce more sensible estimates of uncertainty for observations at the extremes of the scale [see @LinzerStaton2015a, p. 229], the DCPO model places bounds on its estimates of public opinion [@Solt2020a, p. 8].
Further commending the DCPO model to us---and demonstrating that its advantages make a difference---the validation tests in @Solt2020a [pp. 10-12] reveal that it fits survey data on democratic support better than the @Claassen2019 model does.

We employ the superior DCPO model to our expanded dataset using the `DCPO` package for R [@Solt2020].
We then use the estimated public support from the DCPO model to replicate all of the analyses presented in the text.
Figures \@ref(fig:plot-dcpoAJPS) and Figures \@ref(fig:plot-dcpoAPSR) display these results as the righthand set of results, with the replication of the articles' original results based only on point estimates on the left and our extension with uncertainty and more data, but the articles' original, @Claassen2019, model in the middle for comparison.

Even with the advantages of the DCPO model, there is no evidence to support the conclusions of Claassen [-@Claassen2020a; -@Claassen2020] that public support sustains democratic regimes or that public support responds thermostatically to changes in democracy.














\begin{figure}

{\centering \includegraphics{dcpo_demsupport_files/figure-latex/plot-dcpoAJPS-1} 

}

\caption{The Effect of Public Support on Democracy}(\#fig:plot-dcpoAJPS)
\end{figure}

\begin{table}[H]

\caption{(\#tab:num-dcpoAJPS)The Effect of Public Support on Democracy (Uncertainty \& DCPO)}
\centering
\fontsize{8}{10}\selectfont
\begin{tabular}[t]{lcccc}
\toprule
  & Pooled & Pooled-Regime & GMM & GMM-Regime\\
\midrule
Democracy (t-1) & 0.814 & 0.816 & 0.868 & 0.868\\
 & (0.068) & (0.073) & (0.041) & (0.044)\\
Democracy (t-2) & 0.137 & 0.134 & 0.179 & 0.178\\
 & (0.069) & (0.072) & (0.044) & (0.045)\\
Support (t-1) & -0.256 &  & -2.405 & \\
 & (1.961) &  & (2.186) & \\
Support in Democracy &  & -0.203 &  & -2.772\\
 &  & (1.976) &  & (2.208)\\
Support in Autocracy &  & 0.360 &  & -2.109\\
 &  & (1.783) &  & (2.231)\\
Log GDP Per Capita (t-1) & 0.297 & 0.359 & 0.315 & 0.325\\
 & (0.225) & (0.257) & (0.170) & (0.177)\\
GDP Per Capita Growth (t-1) & 0.007 & 0.005 & 0.019 & 0.017\\
 & (0.033) & (0.036) & (0.035) & (0.034)\\
Regional Democracy (t-1) & 0.007 & 0.009 & -0.079 & -0.075\\
 & (0.021) & (0.021) & (0.060) & (0.063)\\
Percent Muslim (t-1) & -0.005 & -0.006 & -0.002 & -0.005\\
 & (0.004) & (0.005) & (0.006) & (0.006)\\
Resource Dependence (t-1) & 0.350 & 0.231 & 0.569 & 0.490\\
 & (0.558) & (0.522) & (0.661) & (0.640)\\
\midrule
N observations & 2792 & 2792 & 5443 & 5443\\
N countries & 141 & 141 & 143 & 143\\
N instruments &  &  & 130 & 130\\
\bottomrule
\end{tabular}
\end{table}







![(\#fig:plot-dcpoAPSR)The Effect of Democracy on the Change of Public Support](dcpo_demsupport_files/figure-latex/plot-dcpoAPSR-1.pdf) 

\blandscape

\begin{table}[H]

\caption{(\#tab:num-dcpoAPSR)The Effect of Public Support on Democracy (Uncertainty \& DCPO)}
\centering
\fontsize{7}{9}\selectfont
\begin{tabular}[t]{lcccccccc}
\toprule
  & ECM & ECM-Regime & FD & FD-Regime & ECM Corrup & ECM Corrup-Regime & FD Corrup & FD Corrup-Regime\\
\midrule
Democratic Mood (t-1) & -0.121 & -0.121 &  &  & -0.129 & -0.131 &  & \\
 & (0.036) & (0.035) &  &  & (0.037) & (0.038) &  & \\
Democratic Mood (t-2) & 0.047 & 0.048 &  &  & 0.048 & 0.050 &  & \\
 & (0.038) & (0.038) &  &  & (0.038) & (0.041) &  & \\
Liberal Democracy (Difference) & 0.000 &  & 0.000 &  & 0.000 &  & 0.000 & \\
 & (0.002) &  & (0.003) &  & (0.002) &  & (0.003) & \\
Liberal Democracy (t-1) & 0.001 &  &  &  & 0.001 &  &  & \\
 & (0.001) &  &  &  & (0.001) &  &  & \\
Electoral Democracy (Difference) &  & 0.001 &  & 0.000 &  & 0.001 &  & 0.001\\
 &  & (0.002) &  & (0.003) &  & (0.002) &  & \vphantom{1} (0.003)\\
Electoral Democracy (t-1) &  & 0.002 &  &  &  & 0.002 &  & \\
 &  & (0.002) &  &  &  & (0.002) &  \vphantom{1} & \\
Minoritarian Democracy (Difference) &  & -0.001 &  & 0.000 &  & -0.001 &  & -0.001\\
 &  & (0.002) &  & (0.003) &  & (0.002) &  & (0.003)\\
Minoritarian Democracy (t-1) &  & -0.001 &  &  &  & -0.001 &  & \\
 &  & (0.002) &  &  &  & (0.002) &  & \\
Log GDP Per Capita (Difference) & 0.009 & 0.009 &  &  & 0.006 & 0.006 &  & \\
 & (0.020) & (0.020) &  &  & (0.022) & (0.023) &  & \\
Log GDP per capita (Difference) &  &  & 0.005 & 0.006 &  &  & 0.006 & 0.005\\
 &  &  & (0.026) & (0.027) &  &  & (0.029) & (0.029)\\
Log GDP (t-1) & 0.001 & 0.001 &  &  & -0.003 & -0.003 &  & \\
 & (0.001) & (0.001) &  &  & (0.002) & (0.002) &  & \\
Corruption (Difference) &  &  &  &  & -0.003 & -0.003 & 0.002 & 0.002\\
 &  &  &  &  & (0.005) & (0.005) & (0.009) & (0.009)\\
Corruption (t-1) &  &  &  &  & -0.006 & -0.006 &  & \\
 &  &  &  &  & (0.002) & (0.002) &  & \\
\midrule
N observations & 2655 & 2655 & 2794 & 2794 & 2386 & 2386 & 2478 & 2478\\
N countries & 141 & 141 & 141 & 141 & 139 & 139 & 139 & 139\\
\bottomrule
\end{tabular}
\end{table}

\elandscape

# References  {-}

<div id = 'refs_app'></div>



\pagebreak

# Declarations {-}

The authors affirm this research did not involve human subjects.

The authors declare no ethical issues or conflicts of interest in this research.
Yue Hu was funded by the National Natural Science Foundation of China (Grant No. 72004109).

Research documentation and data that support the findings of this study are openly available in the APSR Dataverse at https://doi.org/10.7910/DVN/YM67QE.
