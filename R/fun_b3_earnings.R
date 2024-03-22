# [SETUP] -----------------------------------------------------------------
# - Packages (temp) ----------------------------------------------------------------
# CRAN packages
chr_pkg <- c(
  'devtools' #GitHub packages (temp)
  , 'readr' #Read data (temp)
)

# Git packages
chr_git <- c(
)

# Activate / install CRAN packages
lapply(
  chr_pkg
  , function(pkg){

    if(!require(pkg, character.only = T)){

      install.packages(pkg)

    }

    require(pkg, character.only = T)

  }
)

# Activate / install Git packages
Map(
  function(git, profile){

    if(!require(git, character.only = T)){

      install_github(
        paste0(profile, '/', git)
        , upgrade = F
        , force = T
      )

    }

    require(git, character.only = T)

  }
  , git = chr_git
  , profile = names(chr_git)
)

# [FUNCTIONS] --------------------------------------------------------------
# - Calculate dividends function ---------------------------------------------------------
fun_b3_dividends <- function(){

}

# [TEST] ------------------------------------------------------------------
# - Test data -------------------------------------------------------------


# - Function_name ---------------------------------------------------------


