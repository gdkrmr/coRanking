#!/bin/bash


# R_FOLDER=/usr/bin
#R_FOLDER=$HOME/progs/R/R-4.4.1/bin
R_FOLDER=$HOME/progs/R/R-devel/bin

export R_REALLY_FORCE_SYMBOLS=1

export _R_CXX_USE_NO_REMAP_=true 

echo "BUILDING DOCUMENTATION"
$R_FOLDER/Rscript -e 'devtools::document()'

echo "BUILDING VIGNETTES"
$R_FOLDER/Rscript -e 'devtools::build_vignettes()'

echo "REMOVING emacs lockfiles"
find . -type l -exec rm {} \;

echo "BUILDING"
$R_FOLDER/R CMD build .

# echo "LINTING"
# $R_FOLDER/Rscript -e 'lintr::lint_package()'

pkgversion=$(cat DESCRIPTION | grep Version | sed 's|Version: \(.*\)|\1|')
echo "INSTALLING version $pkgversion"
$R_FOLDER/R CMD INSTALL coRanking_$pkgversion.tar.gz

echo "CHECKING!!!"
$R_FOLDER/R CMD check coRanking_$pkgversion.tar.gz --as-cran
