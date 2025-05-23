# Version 0.1.23

22/05/2025 & 23/05/2025

-    `show_SMD` param : provide the standardised mean difference (SMD) between 2 groups + add test

-    `dico_mapping` param renamed as `dico_label`
  
-    Update documentation (fix typo)
  
-    Update vignette with new params' examples

[BUG]

-    `setDT` needed in  `save_excel_paired_results` when selecting with `.SD` operator.
  
-    `save_excel_paired_results_filtertest` skip vars with all NA.

# Version 0.1.22

02/04/2025 & 25/04/2025

-    Shapiro.test not executable with less than 3 point (in overall population, in bivar mode) + add tests (`compute_continuous_table`)

-    `drop_levels` param : drop levels added also on varstrat columns + add test in `save_excel_paired_results` and `save_excel_results`

-    Remove vars with all NA from the vars selection + with a message. Now variables will all Na are removed from the list wanted to be analysed and just stored in a new sheet "Variables_all_na" if applicable + add tests

-    `save_excel_paired_results_filtertest` : new function to use `save_excel_paired_results` in case of missing data + add test

-    vignette fill with new examples of `save_excel_paired_results` in case of missing data : understand how work `save_excel_paired_results_filtertest`.

-    `dico_mapping` param : provide a dataframe to match vars' names and vars' label. "Variable" name will be the first column and "Label" will be set as the 2nd column + add test.

-    Change "N_NA" instead of "NA" col name in compute_ function.

# Version 0.1.21

27/12/2024 - 30/12/2024

[BUG]

-    `compute_paired_continuous_table_and_test` : Fix case when a vars has all values equal (no shapiro test possible, capture error)

[STYLE]

-    N "total" and N missing clarified and Columns names and types homogenized (`Nb_mesures`, `Valeurs_manquantes`, as character)
-    "varstrat1" renamed "varstart"

# Version 0.1.20

20/12/2024 - 23/12/2024

[DOC] 

-   In `save_excel_paired_results` : About 2 *crossed varstrats* : you can provide "var1*var2", and so
     get a description of the 2nd varstrat (var2) for each level of the 1st varstrat (var1).
     Attention, we except var1 as the repeated variables (time or visites) and 
     var2 as the condition/group that will be tested with *not paired test* (if `crossed_varstrat_test` is set TRUE).

-   Update Readme, Authors and Description. 

[BUG]

-    Add simple quote around variables used in `stats::as.formula()`. 

# Version 0.1.19

11/12/2024 - 12/12/2024 - 17/12/2024

[BUG] 

-   In `compute_paired_continuous_table_and_test` : Fix "cell_content" obj in quade case. Fix situation of empty quanti var selected (`dcast` fails after `na.omit` so add early return). Add messages.

-   In `save_excel_paired_results` : When cross varstrats, Fix vars' name when spaces are replaced by dot in "analyse_desc_quali" (after `compute_factorial_table` step).

-   In `save_excel_results` : When cross varstrats, only in quanti sheet, when global_summary = TRUE, reorder "Population totale", just after missing data, and before groups.

-   In `compute_paired_factorial_table_and_test` : Set col order in with Vars names and Modalites first.

-   Add unit tests according above situations.

# Version 0.1.18

02/12/2024

-   Update documentations

-   Update unit tests

[BUG] 

-   Fix crossed_varstrat formating section when quali or quanti vars are not present (test tab is null),
    Fix crossed_varstrat padj too

-   Means difference after - before : 
      in test mean function `test$estimate[2] - test$estimate[1]`
      and in compute paired table `diff <- vec2 - vec1`

-   merge sort = FALSE also in quanti sheets 

[DEV ENVIR] 21/11/2024

-   renv activation not possible (for now, due to connection) in our environment 
  (activation fail so comment the line # source("renv/activate.R") in Rprofile),  
  but fill renv.lock and install deps inside the project 
  (execpt "RVAideMemoire" than fail because of BiocManager's version).

-   Remove vignette because it makes fail the build step (need `devtools::build(args = "--no-build-vignettes")`)
    Just knit it in `inst/` as html doc.

# Version 0.1.17

13/11/2024 

[BUG]

-   Fix OR p-values : in `get_OR_univar`, keep 10 digits for the pvalue and fix display when "p<" keep 0.0000000001.

-   about continuous varstrat :

    - in quanti sheet : add varstrat name before correlation.
    - in quali sheet : show only mead or median according Student T test or Wilcoxon.
    - in quali sheet : if n in a group is 0, show « / » not NaN.

-   Add a vignette

# Version 0.1.16

07/10/2024 - 22/10/2024

-   add unit test :

    -   OR compute
    -   compute paired tables
    -   compute_correlation_table (in compute tables)
    -   save_excel_paired_results : test content
    -   var crossed

-   p.adjust : in fine, when quanti and quali tables are ready to be save, 
  add option to get corrected p values for all vars tested for a given varstrat.

    -   in classic function
    -   same in paired function
    -   roxygenize
    -   format p values as \<0.0001 when applicable.
    -   pvals as numeric format with dec = "." --done in R, but dec = "," in excel

- about test_means :

    - Fix bug when varstrat have less than 2 levels, no test possible
    - Fix rownames with all vars when "L'effectif est < 4 dans au moins l un des groupes "

-   in general

    -   remove "message" if "No." by nothing ""

# Version 0.1.15

04/10/2024

-   about paired function :

    -   add simplify
    -   in case of 2 paired-group, if no data in one group (ex : measure not relevant for contole but needed for cases), keep the vars, skip difference description, show just stat desc, do not deploy test.
    -   fix global_summary option
    -   fix n by visits for continuous vars
    -   fix McNemar's Chi-squared test : only to applied on varstrat with 2 levels and
    -   fix MH test : only to applied on balanced design

-   about var crossed : in classic and paired functions

    -   drop levels,
    -   add in option presence of stat test
    -   remove detail of counts N (N1 + N2 + N3...) =\> become an option

# Version 0.1.14

01/10/2024

-   OR :

    -   P_OR in quali sheet to put after OR
    -   message column at the very end
    -   OR sample size not more with the limite of 3. Juste keep the detection of 0 in TC or NA\|Inf in IC

-   about paired function :

    -   only in paired function : if more than 2 levels in varstrat, remove stat test (repated anova, quade and skilling mack), pass as an option with big warning to test this dev before trust it
    -   in `Difference_description` : mean and median to choose according the final test
    -   mouv `Difference_description` after the 2 group cols
    -   "Nb_measures_init", "Nb_measures", "Nb individuals": column to remove (but keep N in each visit)

-   about N by levels : to modified in paired quanti sheet

# Version 0.1.13

30/09/2024

[BUG]

-   Fix OR when vars have all NA and levels = 0. also add condition about OR shown (sample size \>=3, no zero in contingency table, no NA or Inf in its IC)
-   Fix light_contents when vars have all NA and levels = 0.
-   Fix rbindlist when vars have all NA and levels = 0.
-   about paired function : is_Normal : to keep only if varstrat is NULL. Otherwise done to know if we show mean or median but not shown at the end

# Version 0.1.12

27/09/2024 - 30/09/2014

-   about compute table :
    -   fix shapiro capture error in continuous table and correlation
-   about OR compute :
    -   droplevels so skip error "contrasts can only be applied to factors with at least two levels"
    -   if no more variable present for OR computation, return NULL
-   about paired function :
    -   Marginal Homogeneity Test
    -   [BUG] modify the contingency table given to McNemar test
    -   [BUG] modify p computation by `N_by_visit`
    -   Remove Cochran from paired function
    -   "cochran condition" : column to remove

# Version 0.1.11

19/09/2024 - 20/09/2024

Major improvements :

-   add univariate OR in option `get_OR`
-   new params `signif_digits` : A integer, Default 4. Integer indicating the number of decimal places (signif) for pvalues. different from `digits` which is used to round descriptive statistics.
-   "global_summary" option : keep or not stats of "Population_totale"" col in save_excel functions, like in paired function.

Fix :

-   format : spaces between ";" in "[Q1;Q3]".
-   format : consistency ("Population_totale", "Variable", "Nb_mesures", "P_valeur", "Test", Rename excel sheets)
-   column selection : with varstrat, get Population_totale before group stats.
-   stopifnot(digits \>= 0)

# Version 0.1.10

13/09/2024

-   Update README with more data exemple.

Paired function :

-   fix description if a continuous vars is only available in 1 level visit.

-   fix stat test when a factorial vars is present only in 1 visit, no paired test applicable.

# Version 0.1.9

12/09/2024

-   Meeting biostat : list of change and corrections:

Main function :

-   retirer fonction save_excel initial et n’utiliser que `optim`, et la renomer à la place `save_excel_results`.

-   retirer mot « resp. »

-   renommer ”Metric_show” param names, with values possible: auto, mean, median.

-   retirer option graph

-   remove nlevel max

-   harmoniser light content aussi pour les noms de variables à ne mettre qu'une fois.

-   force_generate_1_when_0 as TRUE by default

-   when varstrat continuous, remove stat desc, only corr estimation with corr test.

-   varstrat_nested =\> “crossed” =\> remove the logical param ad instead detect "*" in “varstrat” param =\> if “var*var” , go in cross levels selection,

-   and when varstrat crossed, do not apply stat test (like for paired function)

-   ordonner les colonnes de fin : D’abord pval, puis test, retirer decision, puis message

-   if force non param, force metric show = median (+message)

-   stopifnot length(varstrat) == 1

-   when varstrat continuous, sheet « quali quanti » avec varstrat conti, retirer « variables », will be in table names. Et simplifier varstrat (ne pas répéter le nom)

-   compute correction function : method “auto detect” according shapiro on the 2 vars =\> if both normal, selection method pearson (remove test lm’s R2 test)

=\> in consequence, think to update roxygen code, documentation, Namespace and unittests

Paired function :

-   stopifnot length(varstrat) == 1

-   if force non param, force metric show = median (+message)

-   varstrat_nested =\> “crossed” =\> remove the logical param ad instead detect “*” in “varstrat” param =\> if “var*var” , go in cross levels selection,

-   add desc temp2 – temp1 only when 2 levels in visit varstrat.

-   vars param to rename according `save_excel_results`

-   global_summary = FALSE by default

-   renommer ”Metric_show” param names, with values possible: auto, mean, median.

-   var of interest must be setdiff with var strat.

-   "keep missing line" option to fix

-   factorial table to fix display when NAN% (like in basic function) if 0 in the whole col

-   fix desc if a vars is only available in 1 level visit, bug.

-   discussion with cassandra : N count on level without missing N, but in Missing data line, N will be the global sum of N levels et N missing.

=\> in consequence, think to update roxygen code, documentation, Namespace and unittests

# Version 0.1.8

06/09/2024

-   Fill Readme with latest option 0.1.7
-   [BUG]
    -   Fix lapply in `save_excel_results_optim` for varstrat NULL.
    -   Fix light_contents in `save_excel_results_optim` for varstrat NULL and for varstrat without sort.
    -   Fix stopifnot in `compute_correlation_table`
    -   Fix typo

# Version 0.1.7

30/08/2024 - 04/09/2024

-   Optimisation of excel core function : `save_excel_results_optim` (going to replace `save_excel_results` after some validation)
    -   add possibility to `varstrat_nested` : 2nd varstrat is described and tested for each level of 1st varstrat.
    -   add varstrat continuous : correlation + its IC95 or contingency table transpose
    -   add option `light_contents`
-   Improve `save_excel_paired_results` for more consistency with `save_excel_results_optim` :
    -   add possibility to `varstrat_nested`
    -   add option `light_contents`
-   [BUG]
    -   Fix `varstrat[i]` in `save_excel_results`
    -   Fix message with function names (help to debug)
    -   Fix `tab_quali_sheet` for column name starting with a number in `save_excel_results_optim`

# Version 0.1.6

28/08/2024 - 29/08/2024

-   On univariate or bivariate quantitative analyses : keep info of normality (`compute_continuous_table`) (`is_Normal` column ever exist in paired function)
-   On bivariate quantitative analyses (when `varstrat` is not NULL) : add a param (`save_excel_paired_results` and `save_excel_results`) to select `quali_metric_to_show` as `1-mean`, `2-median` or `3-auto` meaning resp. 1 = moyenne +/- quanti, 2 = mediane [Q1;Q3], 3 = automatique according statistical test applied.
-   Add prefix about {stats} r pkg everywhere to avoid conflict with {coin}
-   In paired functions, fix capture message for wilcoxon `has_issues_w` and `force_non_parametric_test` option like for `save_excel_paired_results`

# Version 0.1.5

28/08/2024

-   Add paired functions, see `compute_paired_factorial_table_and_test.R` : McNemar or Marginal Homogeneity Test (just prepared, never test, need to found toydata to write it)

# Version 0.1.4

09/08/2024 - 27/08/2024

Improvements

-   Rename "save_excel_report" function as "save_excel_results"
-   Add paired functions, see `save_excel_paired_results` and `compute_paired_continuous_table_and_test.R` : Paired t test, Wilcoxon signed-rank test (paired data), Quade, Skilling-Mack, Paired anova. Add unit test.
-   Force to use non parametric test, whatever shapiro said (option `force_non_parametric_test`) and add a warning when normality is tested on too few points. Add unit test.
-   Fill Readme
-   [BUG]
    -   Fix get_factors and get_numerics about vars selection
    -   Fix prefix
    -   Update Imports in DESCRIPTION file
    -   Import data.table operators (see <https://cran.r-project.org/web/packages/data.table/vignettes/datatable-importing.html#globals>) : automatic creation of "R2Excel-package.R" script.

`usethis::use_import_from(package = "data.table", fun = ".SD")`\
`usethis::use_import_from(package = "data.table", fun = ".N")`\
`usethis::use_import_from(package = "data.table", fun = ":=")`

# Version 0.1.3

08/08/2024

-   Proportion table : order levels according varstrat original labels' order.

# Version 0.1.2

07/08/2024 - 08/08/2024

-   [BUG]
    -   Fix major bugs : fix the test deployed in quali workbook = test_proportion to use instead of test_mean (put inadvertently).
    -   Fix NAN % by testing is.nan in prop.table =\> show 0% now when a modality is not present for a level of varstrat.

Improvements

-   Add unit tests about content of excel workbook
-   If shapiro test has issues, still show the descriptive values
-   Multiple varstart possible (lapply)
-   Add option in `compute_factorial_table` to `force_reverse_0_into_1` which turn also "no" into "yes" when only "no" is observed.
-   Add fisher exact test "good application" condition `check_fisher`.
-   Fill the Readme with current example of use.

# Version 0.1.1

01/08/2024 - 06/08/2024

-   Add unit tests

-   Clarify names in toy data "modified_state" and add documentation

-   {Styler}

-   Loop on multiple varstrat possible (one excel sheet per varstrat), fix excel sheet name in consequence

-   Fix varstrat previously called "var" (error)

-   Fix typo

# R2Excel 0.1.0

31/07/2024

-   Initial build.
