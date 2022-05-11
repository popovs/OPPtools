# OPPtools

<!--<a href='https://cida-csph.github.io/CIDAtools'><img src='inst/figures/CIDAtoolshex.png' align="right" height="139" /></a>

[![R-CMD-check](https://github.com/CIDA-CSPH/CIDAtools/workflows/R-CMD-check/badge.svg)](https://github.com/CIDA-CSPH/CIDAtools/actions)
-->

## Overview

This package provides functions and Rmarkdown templates for analyzing marine bird tracking data collected for the Oceans Protection Plan. The package implements a workflow for identifying high-use areas around colonial breeding seabird colonies on he coast of British Columbia, using GPS tracking data strewed on the Movebank animal tracking data repository.

The package provides functions for data formatting, trip identification, track interpolation, calculating utilization distributions, and identifying high-use areas where multiple individual tracks overlap. The work flow draws on existing functions in the 'move', 'adehabitatHR', and 'track2KBA' R packages. The package includes two Rmarkdown templates to produce (1) a diagnostic report summarizing tracking data within a project and (2) a final report on high-use areas with accompanying shapefile exports.

The package is designed so that reports can be esaily updated as new tracking data and projects are added to Movebank.

## Installing package

To install `OPPtools` on your local machine:

```
# install.packages("devtools")
devtools::install_github('popovs/OPPtools')

```

## Projects on Movebank

Currently, there are 15 OPP projects on Movebank. Data collected on colonial breeding marine birds are organized by species and colony. Data from non-colonial species and from non-breeding birds are organized by species. A list of Movebank project IDs can be called as a dataset within the package using:

```
# List of Movebank project IDs
opp_mb_projects
```

## Importing data from Movebank

Before importing data from Movebank, you will need to create a Movebank user account at https://www.movebank.org/cms/movebank-login and be added as a Data collaborator or Data manager by the Project Owner or Project Contact. You may initiate a request to be added to a project through Movebank or by contacting the data owner directly.

Within OPPtools the function `opp_movebank_key()` uses the keyring package to add your Movebank credentials to your keychain. This will allow any functions that call Movebank data to link to Movebank without having to re-enter your credentials.

Functions within OPPtools that call Movebank data are wrappers to the `move` package. If you want to pass your credentials directly to functions from the move package, you can use:

```
mb_login <- opp_retrieve_mb_cred()
```



## Creating a new project

After `OPPtools` is installed, a new OPP project can be created using the
Rstudio GUI (File>New Project>New Directory>New OPP Project).

Or, to create a new project folder structure from the R console, 
navigate to the main project directory, and run: 

```
CreateProject()

```

New projects will have a standardized .Rproj directory with the following preset subdirectories:

1. 01_Raw_data
2. 02_Analysis
3. 03_Working_files
4. 04_Processed
5. 05_Admin

In addition, a README.md file will be generated for all newly created directories to describe which files are contained within.

Feel free to set the project name, PI, and analyst via the `ProjectName`, `PI`, or `analyst`
arguments respectively, and see `?CreateProject` for more details. If any of these are
specified, they are stored as text in a hidden subdirectory called .ProjData, and can
be called in subsequent R code anywhere in the project using `ProjectName()`, `ProjectPI()`, and
`ProjectAnalyst()`. 

```

