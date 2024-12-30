# R2Excel

<!-- badges: start -->
<!-- badges: end -->

The goal of *R2Excel* is to produce Statistical Tables in *R*, Save Into (*2*) *Excel* File. 

With this package, we don't propose any revolutionary statistical models or tests, but we do recommend that you use this tool for the first stages of analysis in a clinical study. Once you have your databases, you will naturally want to produce descriptive tables, and the first univariate or bivariate tests (if groups are present). We can help you to standardise these tables, while using the appropriate tests (implementation and verification of certain hypotheses, paired tests or captured messages). The main functions produce descriptive, homogeneous and harmonious statistical tables, as well as statistical tests.  

This package is used to generate statistical reports produced by the Methodology/Biostatistics Team of the GHICL DRCI (Lomme, France).

Feel free to participate to this project : It is designed to be open source under a [CECILL-2 Licence](https://cecill.info/licences/Licence_CeCILL_V2.1-en.txt) (French equivalent of the GPL license).
Any improvements or help with the documentation are welcome. Please create a branch to submit your merge request. 

French note : 

L'objectif de *R2Excel* est de produire des tableaux statistiques dans *R* et de (*2*) l'enregistrer dans un fichier *Excel*. 

Avec ce package, nous ne sommes pas en train de vous proposer des modèles ou tests statistiques révolutionnaires, mais nous vous recommandons d'utiliser cet outil pour les premières étapes de l'analyse d'une étude clinique. Lorsque vous disposez des bases de données, vous souhaitez naturellement produire des tables descriptives, ainsi que ls premiers tests univariés ou bivariés (si présence de groupes). Nous vous proposons de standardiser ces tables tout en utilisant les tests adéquats (implémentation et vérification de certaines hypothèses, tests appariés ou encore messages capturés). 

Les fonctions principales produisent des tableaux statistiques descriptifs, homogènes et harmonieux ainsi que des tests statistiques.  

Ce package est utilisé pour générer les rapports statistiques produits par la cellule Méthodologie/Biostatistique du GHICL DRCI (Lomme, France).

N'hésitez pas à participer à ce projet : il est conçu pour être open source sous une [Licence CECILL-2](https://cecill.info/licences/Licence_CeCILL_V2.1-fr.txt) (équivalent français de la licence GPL). 
Toute amélioration ou aide à la documentation est la bienvenue. Pour ce faire, nous vous prions de bien vouloir procéder à la création d'une branche pour nous soumettre votre requête ("merge request"). 


## Authors

in alphabetic order 

  Mathilde Boissel [aut, cre],  
  Cassandra Chaldaureille [aut, cre],  
  Sahara Graf [aut],  
  Laurène Norberciak [aut, cre],  
  Cristian Preda [aut, cre],  
  Stephane Verdun [aut]  

## Installation

Directly from GitHub, 

```r
# install.packages("devtools")
devtools::install_github("GHICL-DRCI/R2Excel")
```

Or manually,  

From the online repository : `https://github.com/GHICL-DRCI/R2Excel/`,  
Download the zipped repo : `> code > Download Zip`,  
On your machine, on R, `setwd("where_your_zip_is/");`,  
`unzip("R2Excel-master.zip");`, `file.rename("R2Excel-master", " R2Excel");`,  
Build it `shell("R CMD build R2Excel ")` (will produce the [pkg].tar.gz),  
Then install it from your local build, `install.packages("R2Excel_[version].tar.gz", repos = NULL)`  

Check your current version with `packageVersion("R2Excel")`

Usefull command to know what's new : `utils::news(package = "R2Excel")`
