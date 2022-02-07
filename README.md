
# dimensionsR

<!-- badges: start -->
<!-- badges: end -->

## An R-package to gather bibliographic data from DS Dimenions.ai

[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![Build Status](https://www.bibliometrix.org/passing.png)](https://github.com/massimoaria/dimensionsR)
[![rstudio mirror downloads](https://cranlogs.r-pkg.org/badges/dimensionsR)](https://github.com/metacran/cranlogs.app)
[![cran version](http://www.r-pkg.org/badges/version/dimensionsR)](https://cran.r-project.org/package=dimensionsR)

&nbsp;

    The goal of dimensionsR is to gather metadata about publications, grants, policy documents,
    and clinical trials from DS Dimensions using DSL API.

<!-- badges: start -->
<!-- badges: end -->


&nbsp; 

*http://github.com/massimoaria/dimensionsR*


&nbsp; 

**by Massimo Aria**

Full Professor in Social Statistics 

PhD in Computational Statistics

Laboratory and Research Group STAD Statistics, Technology, Data Analysis

Department of Economics and Statistics 

University of Naples Federico II

email aria@unina.it

https://www.massimoaria.com

&nbsp; 

## Installation

You can install the developer version of the dimensionsR from [GitHub](https://github.com) with:

    install.packages("devtools")
    devtools::install_github("massimoaria/dimensionsR")



You can install the released version of dimensionsR from [CRAN](https://CRAN.R-project.org) with:

    install.packages("dimensionsR")

&nbsp; 

&nbsp; 


## Load the package


```{r}
library(dimensionsR)
```


&nbsp; 

&nbsp; 


#  A brief example

Imagine, we want to download a metadata collection of journal articles which (1) have used bibliometric approaches in their researches, (2) have been published for the past 20 years (3) and have been written in the English language. 

The workflow mainly consists of five steps:

1. Generate a valid token

2. Write the query

3. Check the effectiveness of the query

4. Download the collection of document metadata

5. Convert the download object into a "readable" and and "usable" format

&nbsp;

## First step: Generate a valid token

The access to DS Dimensions API system (https://www.dimensions.ai/) requires an "API token". Dimensions provides free data access for scientometric research projects, but you have to submit a request to obtain valid credentials (to generate API tokens) at https://digital-science.ccgranttracker.com/.

Once your request is approved, you can generate a valid token using the function ***dsAuth*** with your dimensions credentials or your dimensions API key.

### Token by Dimensions credentials:

    token <- dsAuth(username = "your_username", password = "your_password")

### Token by Dimensions API key:

    token <- dsAuth(key = "your_apikey")

The token is temporary and needs to be generated again after a few hours. 

&nbsp; 


## Second step: Write a valid query

Dimensions defined a custom query language called Dimensions Search Language (DSL). You can choose to write a valid query using that language or, in alternaative, using the function ***dsQueryBuild***.

***dsQueryBuild*** generates a valid query, written following the Dimensions Search Language (DSL), from a set of search parameters provided by the user. 

Following our example, we have to define a query to download metadata about: (1) a set of journal articles which (2) have used bibliometric approaches in their researches, (4) in the field of management sciences, (3) and have been published for the past 20 years. 

To write that query, we should set the function arguments as following:

- *a set of journal articles* 
    
    **item = "publications"**
    
    **type = "articles"**

- *have used bibliometric approaches in their researches*
  
    **words = "bibliometric\*"**

- *in the field of management sciences* 

    **categories = "management"**

- *documents published from 2000 to 2020:* 

    **start_year = 2000**
    
    **end_year = 2020**

- *and finally, export the following fields*

    **output_fields = c("basics", "extras", "authors", "concepts")**
    
    (you could also export all database fields setting output_fields = "all")

Passing all these arguments to the function ***dsQueryBuild***, we obtain the final query:


    query <- dsQueryBuild(item = "publications", 
                      words = "bibliometric*", 
                      type = "article", 
                      categories = "management", 
                      start_year = 1980, end_year = 2020,
                      output_fields = c("basics", "extras", "authors", "concepts"))
                      
    query
    
    # "search publications in title_abstract_only  for \"\\\"bibliometric*\\\"\" where year in [ 1980 : 2020 ] and type in [ \"article\" ] and (category_for.name ~\"management\") return publications[type + basics + extras + authors + concepts]"

A complete list of output items is available at https://docs.dimensions.ai/dsl/data-sources.html
&nbsp; 

## Third step: Check the effectiveness of the query

Now, we want to know how many documents could be retrieved by our query. 

To do that, we use the function dsApiRequest, setting the argument limit = 0:


    res <- dsApiRequest(token = token, query = query, limit = 0)

    res$total_count

    # [1] 1148


&nbsp; 

## Fourth step: Download the collection of document metadata

We could decide to change the query or continue to download the whole collection or a part of it (setting the limit argument lower than res$total_count).

Image, we decided to download the whole collection composed by 1148 documents:


    D <- dsApiRequest(token = token, query = query, step = 200, limit = res$total_count)

    # Records:  200 of 1148 
    # Records:  400 of 1148 
    # Records:  600 of 1148 
    # Records:  800 of 1148 
    # Records:  1000 of 1148 
    # Records:  1148 of 1148 


The function dsApiRequest returns a list D composed by 4 objects:

- "data". It is the xml-structured list containing the bibliographic metadata collection downloaded from the DS Dimensions database.

- "item". It idicates the type of record retrieved. The argument can be equal to  *c("publications", "grants", "patents", "clinical_trials", "policy_documents")*. 

- "query". It a character object containing the query formulated with dsQueryBuild (or by the user) in the DSL language.

- "total_counts". It is an integer object indicating the total number of records matching the query. 


&nbsp; 

## Fifth step: Convert the download object into a "readable" and and "usable" format


### From the xml-structured object to a "classical" data frame

Finally, we transform the xml-structured object D into a data frame, with cases corresponding to documents and variables to Field Tags as used in the **bibliometrix R package** (https://CRAN.R-project.org/package=bibliometrix, https://bibliometrix.org/, https://github.com/massimoaria/bibliometrix).


    M <- dsApi2df(D)

    str(M)

    'data.frame':	1148 obs. of  36 variables:
    # $ AU     : chr  "DUQUE-ACEVEDO M;BELMONTE-UREÑA LJ;CORTÉS-GARCÍA FJ;CAMACHO-FERRE F" "SINGH S;DHIR S;DAS VM;SHARMA A" "FARINHA L;SEBASTIÃO JR;SAMPAIO C;LOPES J" "BILDOSOLA I;GARECHANA G;ZARRABEITIA E;CILLERUELO E" ...
    # $ AF     : chr  "DUQUE-ACEVEDO, MÓNICA;BELMONTE-UREÑA, LUIS J.;CORTÉS-GARCÍA, FRANCISCO JOAQUÍN;CAMACHO-FERRE, FRANCISCO" "SINGH, SHIWANGI;DHIR, SANJAY;DAS, V. MUKUNDA;SHARMA, ANUJ" "FARINHA, LUÍS;SEBASTIÃO, JOÃO RENATO;SAMPAIO, CARLOS;LOPES, JOÃO" "BILDOSOLA, IÑAKI;GARECHANA, GAIZKA;ZARRABEITIA, ENARA;CILLERUELO, ERNESTO" ...
    # $ TI     : chr  "AGRICULTURAL WASTE: REVIEW OF THE EVOLUTION, APPROACHES AND PERSPECTIVES ON ALTERNATIVE USES" "BIBLIOMETRIC OVERVIEW OF THE TECHNOLOGICAL FORECASTING AND SOCIAL CHANGE JOURNAL: ANALYSIS FROM 1970 TO 2018" "SOCIAL INNOVATION AND SOCIAL ENTREPRENEURSHIP: DISCOVERING ORIGINS, EXPLORING CURRENT AND FUTURE TRENDS" "CHARACTERIZATION OF STRATEGIC EMERGING TECHNOLOGIES: THE CASE OF BIG DATA" ...
    # $ SO     : chr  "GLOBAL ECOLOGY AND CONSERVATION" "TECHNOLOGICAL FORECASTING AND SOCIAL CHANGE" "INTERNATIONAL REVIEW ON PUBLIC AND NONPROFIT MARKETING" "CENTRAL EUROPEAN JOURNAL OF OPERATIONS RESEARCH" ...
    # $ SO_LIST: chr  "DOAJ;ERA 2018;NORWEGIAN REGISTER LEVEL 1" "NORWEGIAN REGISTER LEVEL 1;ERA 2015;VABB-SHW;ERA 2018" "ERA 2015;ERA 2018" "NORWEGIAN REGISTER LEVEL 1;ERA 2015;VABB-SHW;ERA 2018" ...
    # $ LA     : chr  "ENGLISH" "ENGLISH" "ENGLISH" "ENGLISH" ...
    # $ DT     : chr  "ARTICLE" "ARTICLE" "ARTICLE" "ARTICLE" ...
    # $ DE     : chr  "RESEARCH;ANALYSIS;SCIENTIFIC PRODUCTION;PRODUCTION;ARTICLE;BIBLIOMETRIC METHODS;EVOLUTION;APPROACH;TRENDS;TRANS"| __truncated__ "PURPOSE;PAPER;TECHNOLOGICAL FORECASTING;JOURNALS;PERIOD;ARTICLE;SCOPE;DIVERSITY;FIELD;FRAGMENTATION;BELIEFS;REV"| __truncated__ "NONPROFIT SECTOR;SECTOR;IMPORTANT PART;PART;ECONOMY;CRITICAL FACTOR;FACTORS;SOCIAL CHANGE;CHANGES;SOCIAL INNOVA"| __truncated__ "CURRENT ENTERPRISES;ENTERPRISES;EMERGING TECHNOLOGIES;TECHNOLOGY;LATE ADOPTION;ADOPTION;COMPETITIVENESS;FRAME;A"| __truncated__ ...
    # $ ID     : chr  "RESEARCH;ANALYSIS;SCIENTIFIC PRODUCTION;PRODUCTION;ARTICLE;BIBLIOMETRIC METHODS;EVOLUTION;APPROACH;TRENDS;TRANS"| __truncated__ "PURPOSE;PAPER;TECHNOLOGICAL FORECASTING;JOURNALS;PERIOD;ARTICLE;SCOPE;DIVERSITY;FIELD;FRAGMENTATION;BELIEFS;REV"| __truncated__ "NONPROFIT SECTOR;SECTOR;IMPORTANT PART;PART;ECONOMY;CRITICAL FACTOR;FACTORS;SOCIAL CHANGE;CHANGES;SOCIAL INNOVA"| __truncated__ "CURRENT ENTERPRISES;ENTERPRISES;EMERGING TECHNOLOGIES;TECHNOLOGY;LATE ADOPTION;ADOPTION;COMPETITIVENESS;FRAME;A"| __truncated__ ...
    # $ AB     : chr  "NA" "NA" "NA" "NA" ...
    # $ C1     : chr  "UNIVERSITY OF ALMERÍA, SPAIN;UNIVERSITY OF ALMERÍA, SPAIN;UNIVERSIDAD AUTÓNOMA DE CHILE, CHILE;UNIVERSITY OF ALMERÍA, SPAIN" "DEPARTMENT OF MANAGEMENT STUDIES, INDIAN INSTITUTE OF TECHNOLOGY (IIT) DELHI, HAUZ KHAS, NEW DELHI - 110016, IN"| __truncated__ "POLYTECHNIC INSTITUTE OF CASTELO BRANCO AND NECE RESEARCH CENTER IN BUSINESS SCIENCES, LARGO DO MUNICÍPIO, 6060"| __truncated__ "UNIVERSITY OF THE BASQUE COUNTRY, SPAIN;UNIVERSITY OF THE BASQUE COUNTRY, SPAIN;UNIVERSITY OF THE BASQUE COUNTR"| __truncated__ ...
    # $ RP     : chr  "UNIVERSITY OF ALMERÍA,SPAIN" "DEPARTMENT OF MANAGEMENT STUDIES, INDIAN INSTITUTE OF TECHNOLOGY (IIT) DELHI, HAUZ KHAS, NEW DELHI - 110016, INDIA,NA" "POLYTECHNIC INSTITUTE OF CASTELO BRANCO AND NECE RESEARCH CENTER IN BUSINESS SCIENCES, LARGO DO MUNICÍPIO, 6060"| __truncated__ "UNIVERSITY OF THE BASQUE COUNTRY,SPAIN" ...
    # $ OI     : chr  "0000-0001-5860-5000;0000-0003-0209-0997" "0000-0001-8693-5947" "" "0000-0002-4964-9762;0000-0002-1913-3239;0000-0002-2347-3885" ...
    # $ FU     : chr  "" "" "" "" ...
    # $ CR     : chr  "PUB.1026056866;PUB.1033294208;PUB.1022712617;PUB.1049702372;PUB.1008318289;PUB.1121499517;PUB.1120414045;PUB.11"| __truncated__ "PUB.1059655231;PUB.1069907290;PUB.1009741249;PUB.1028082114;PUB.1026178255;PUB.1023011736;PUB.1035913069;PUB.10"| __truncated__ "PUB.1067499492;PUB.1027037063;PUB.1026026695;PUB.1101848308;PUB.1037291488;PUB.1049249540;PUB.1041384155;PUB.10"| __truncated__ "PUB.1040547089;PUB.1009922941;PUB.1009413848;PUB.1109700874;PUB.1019443976;PUB.1048606603;PUB.1017000773;PUB.10"| __truncated__ ...
    # $ ALT    : chr  "2" NA NA "1" ...
    # $ TC     : num  1 0 0 1 2 3 0 0 0 0 ...
    # $ TCR    : num  1 0 0 1 2 3 0 0 0 0 ...
    # $ PU     : chr  "ELSEVIER" "ELSEVIER" "SPRINGER NATURE" "SPRINGER NATURE" ...
    # $ SN     : chr  "2351-9894" NA NA NA ...
    # $ J9     : chr  NA NA NA NA ...
    # $ JI     : chr  NA NA NA NA ...
    # $ PY     : num  1982 1982 1982 1982 1982 ...
    # $ VL     : chr  "22" "154" "17" "28" ...
    # $ IS     : chr  NA NA "1" "1" ...
    # $ DI     : chr  "10.1016/j.gecco.2020.e00902" "10.1016/j.techfore.2020.119963" "10.1007/s12208-020-00243-6" "10.1007/s10100-018-0597-9" ...
    # $ PG     : chr  "E00902" "119963" "77-96" "45-60" ...
    # $ SC     : chr  "ENVIRONMENTAL SCIENCES; BIOLOGICAL SCIENCES; ECOLOGY; ENVIRONMENTAL SCIENCE AND MANAGEMENT" "TECHNOLOGY; ECONOMICS; COMMERCE, MANAGEMENT, TOURISM AND SERVICES" "MARKETING; COMMERCE, MANAGEMENT, TOURISM AND SERVICES" "COMMERCE, MANAGEMENT, TOURISM AND SERVICES; APPLIED MATHEMATICS; BUSINESS AND MANAGEMENT; NUMERICAL AND COMPUTA"| __truncated__ ...
    # $ OA     : chr  "ALL OA" "CLOSED" "CLOSED" "CLOSED" ...
    # $ URL    : chr  "https://doi.org/10.1016/j.gecco.2020.e00902" NA NA NA ...
    # $ DB     : chr  "DIMENSIONS" "DIMENSIONS" "DIMENSIONS" "DIMENSIONS" ...
    # $ AU_UN  : chr  "UNIVERSITY OF ALMERÍA;UNIVERSITY OF ALMERÍA;UNIVERSIDAD AUTÓNOMA DE CHILE;UNIVERSITY OF ALMERÍA" "DEPARTMENT OF MANAGEMENT STUDIES, INDIAN INSTITUTE OF TECHNOLOGY (IIT) DELHI, HAUZ KHAS, NEW DELHI - 110016, IN"| __truncated__ "POLYTECHNIC INSTITUTE OF CASTELO BRANCO AND NECE RESEARCH CENTER IN BUSINESS SCIENCES, LARGO DO MUNICÍPIO, 6060"| __truncated__ "UNIVERSITY OF THE BASQUE COUNTRY;UNIVERSITY OF THE BASQUE COUNTRY;UNIVERSITY OF THE BASQUE COUNTRY;UNIVERSITY O"| __truncated__ ...
    # $ AU1_UN : chr  "UNIVERSITY OF ALMERÍA" "DEPARTMENT OF MANAGEMENT STUDIES, INDIAN INSTITUTE OF TECHNOLOGY (IIT) DELHI, HAUZ KHAS, NEW DELHI - 110016, INDIA" "POLYTECHNIC INSTITUTE OF CASTELO BRANCO AND NECE RESEARCH CENTER IN BUSINESS SCIENCES, LARGO DO MUNICÍPIO, 6060"| __truncated__ "UNIVERSITY OF THE BASQUE COUNTRY" ...
    # $ AU_CO  : chr  "SPAIN;SPAIN;CHILE;SPAIN" "" "PORTUGAL;PORTUGAL;PORTUGAL" "SPAIN;SPAIN;SPAIN;SPAIN" ...
    # $ AU1_CO : chr  "SPAIN" NA "PORTUGAL" "SPAIN" ...
    # $ SR_FULL: chr  NA NA NA NA ...


&nbsp; 


## An overview to the collection using bibliometrix

Now, we can use some bibliometrix functions to get an overview of the bibliographic collection.

bibliometrix is an R-tool for quantitative research in scientometrics and bibliometrics that includes all the main bibliometric methods of analysis (https://CRAN.R-project.org/package=bibliometrix, https://bibliometrix.org/, https://github.com/massimoaria/bibliometrix).

First, we install and load the bibliometrix package:


    install.packages("bibliometrix")
    library(bibliometrix)

&nbsp; 

### Main information about the collection

Then, we use the biblioAnalysis and summary functions to perform a descriptive analysis of the data frame: Then, we add some metadata to the dimensions collection, and we use the biblioAnalysis and summary functions to perform a descriptive analysis of the data frame:

    M <- convert2df(D, dbsource = "dimensions", format = "api")

    results <- biblioAnalysis(M)
    summary(results)

        # MAIN INFORMATION ABOUT DATA
        # 
        # Timespan                              1982 : 2020 
        # Sources (Journals, Books, etc)        568 
        # Documents                             1148 
        # Average years from publication        4.81 
        # Average citations per documents       19.54 
        # Average citations per year per doc    2.876 
        # References                            0 
        # 
        # DOCUMENT TYPES                     
        # article      1148 
        # 
        # DOCUMENT CONTENTS
        # Keywords Plus (ID)                    12874 
        # Author's Keywords (DE)                12874 
        #  
        # AUTHORS
        #  Authors                               2783 
        #  Author Appearances                    3538 
        #  Authors of single-authored documents  140 
        #  Authors of multi-authored documents   2643 
        #  
        # AUTHORS COLLABORATION
        #  Single-authored documents             148 
        #  Documents per Author                  0.413 
        #  Authors per Document                  2.42 
        #  Co-Authors per Documents              3.08 
        #  Collaboration Index                   2.66 
        #  
        # 
        # Annual Scientific Production
        # 
        #  Year    Articles
        #     1982        1
        #     1983        1
        #     1985        2
        #     1986        1
        #     1987        3
        #     1988        4
        #     1989        1
        #     1990        2
        #     1992        3
        #     1993        3
        #     1994        3
        #     1995        5
        #     1996        4
        #     1997        3
        #     1998        4
        #     1999       10
        #     2000        3
        #     2001        8
        #     2002        6
        #     2003        1
        #     2004        4
        #     2005       10
        #     2006       17
        #     2007       13
        #     2008       13
        #     2009       16
        #     2010       22
        #     2011       32
        #     2012       32
        #     2013       48
        #     2014       37
        #     2015       68
        #     2016       85
        #     2017      108
        #     2018      147
        #     2019      283
        #     2020      145
        # 
        # Annual Percentage Growth Rate 13.99298 
        # 
        # 
        # Most Productive Authors
        # 
        #    Authors        Articles Authors        Articles Fractionalized
        # 1     MERIGÓ JM         20    MERIGÓ JM                      5.43
        # 2     ZHANG Y           14    KUMAR S                        4.08
        # 3     KUMAR S           13    KOSTOFF RN                     3.69
        # 4     PORTER AL         11    ZHANG Y                        3.47
        # 5     KOSTOFF RN        10    MCMILLAN GS                    3.33
        # 6     WANG J            10    HO Y                           3.25
        # 7     CAPUTO A           9    VOGEL R                        3.17
        # 8     MARZI G            9    FERREIRA MP                    3.00
        # 9     ALON I             8    PASADEOS Y                     3.00
        # 10    FERREIRA MP        8    PORTER AL                      2.94
        # 
        # 
        # Top manuscripts per citations
        # 
        #                                                     Paper            TC TCperYear
        # 1  PARK SH, 1996, STRATEGIC MANAGEMENT JOURNAL                     1011     40.44
        # 2  RAMOS‐RODRÍGUEZ A, 2004, STRATEGIC MANAGEMENT JOURNAL            472     27.76
        # 3  DAIM TU, 2006, TECHNOLOGICAL FORECASTING AND SOCIAL CHANGE       461     30.73
        # 4  FAHIMNIA B, 2015, INTERNATIONAL JOURNAL OF PRODUCTION ECONOMICS  368     61.33
        # 5  DE BAKKER FGA, 2005, BUSINESS & SOCIETY                          359     22.44
        # 6  KOSTOFF RN, 2001, IEEE TRANSACTIONS ON ENGINEERING MANAGEMENT    351     17.55
        # 7  MURRAY F, 2002, RESEARCH POLICY                                  300     15.79
        # 8  ZUPIC I, 2015, ORGANIZATIONAL RESEARCH METHODS                   296     49.33
        # 9  MOED H, 1985, RESEARCH POLICY                                    283      7.86
        # 10 GALVAGNO M, 2014, MANAGING SERVICE QUALITY                       222     31.71
        # 
        # 
        # Corresponding Author's Countries
        # 
        # Country Articles   Freq SCP MCP MCP_Ratio
        # 1  UNITED STATES       116 0.1420  79  37     0.319
        # 2  CHINA               104 0.1273  57  47     0.452
        # 3  BRAZIL               79 0.0967  61  18     0.228
        # 4  SPAIN                69 0.0845  48  21     0.304
        # 5  UNITED KINGDOM       49 0.0600  31  18     0.367
        # 6  ITALY                47 0.0575  34  13     0.277
        # 7  AUSTRALIA            45 0.0551  22  23     0.511
        # 8  GERMANY              34 0.0416  25   9     0.265
        # 9  PORTUGAL             24 0.0294  16   8     0.333
        # 10 NETHERLANDS          22 0.0269  17   5     0.227
        # 
        # 
        # SCP: Single Country Publications
        # 
        # MCP: Multiple Country Publications
        # 
        # 
        # Total Citations per Country
        # 
        # Country      Total Citations Average Article Citations
        # 1  UNITED STATES                     4936                    42.552
        # 2  UNITED KINGDOM                    1654                    33.755
        # 3  SPAIN                             1618                    23.449
        # 4  CHINA                             1472                    14.154
        # 5  AUSTRALIA                         1416                    31.467
        # 6  NETHERLANDS                       1252                    56.909
        # 7  BRAZIL                             985                    12.468
        # 8  ITALY                              890                    18.936
        # 9  GERMANY                            662                    19.471
        # 10 FINLAND                            559                    50.818
        # 
        # 
        # Most Relevant Sources
        # 
        # Sources        Articles
        # 1  TECHNOLOGICAL FORECASTING AND SOCIAL CHANGE           48
        # 2  RESEARCH POLICY                                       44
        # 3  JOURNAL OF BUSINESS RESEARCH                          24
        # 4  TECHNOLOGY ANALYSIS AND STRATEGIC MANAGEMENT          24
        # 5  TECHNOVATION                                          14
        # 6  INDUSTRIAL MARKETING MANAGEMENT                       11
        # 7  INTERNATIONAL JOURNAL OF HOSPITALITY MANAGEMENT       10
        # 8  THE JOURNAL OF TECHNOLOGY TRANSFER                    10
        # 9  IEEE TRANSACTIONS ON ENGINEERING MANAGEMENT            9
        # 10 R AND D MANAGEMENT                                     9
        # 
        # 
        # Most Relevant Keywords
        # 
        # Author Keywords (DE)      Articles Keywords-Plus (ID)     Articles
        # 1      RESEARCH                   683  RESEARCH                   683
        # 2      ANALYSIS                   678  ANALYSIS                   678
        # 3      STUDY                      442  STUDY                      442
        # 4      PAPER                      428  PAPER                      428
        # 5      ARTICLE                    406  ARTICLE                    406
        # 6      BIBLIOMETRIC ANALYSIS      398  BIBLIOMETRIC ANALYSIS      398
        # 7      LITERATURE                 376  LITERATURE                 376
        # 8      JOURNALS                   320  JOURNALS                   320
        # 9      APPROACH                   307  APPROACH                   307
        # 10     AUTHORS                    299  AUTHORS                    299

"# dimensionsR" 
