#!/bin/bash

Rscript -e 'library(roxygen2); roxygenize()'
R CMD build .
