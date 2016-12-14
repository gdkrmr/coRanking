#!/bin/bash


echo "BUILDING DOCUMENTATION"
Rscript -e 'devtools::document()'

echo "BUILDING VIGNETTES"
Rscript -e 'devtools::build_vignettes()'

echo "REMOVING emacs lockfiles"
find . -type l -exec rm {} \;

echo "BUILDING"
R CMD build .

echo "LINTING"
Rscript -e 'lintr::lint_package()'

pkgversion=$(cat DESCRIPTION | grep Version | sed 's|Version: \(.*\)|\1|')
echo "INSTALLING version $pkgversion"
R CMD INSTALL coRanking_$pkgversion.tar.gz

echo "CHECKING!!!"
R CMD check coRanking_$pkgversion.tar.gz --as-cran
