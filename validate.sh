#!bin/bash
Rscript -e "install.packages(c('devtools', 'roxygen2'))"
curl -L http://github.com/data-cleaning/validate/archive/master.tar.gz | tar xz
cd validate-master
bash ./build.sh
R CMD INSTALL pkg
cd ..
rm -rf validate-master
