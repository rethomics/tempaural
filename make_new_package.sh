#To initiate a new package 
TEMPLATE=z_template_package
# all these files are shared between all packages:
template_files=".gitignore  Makefile .Rbuildignore README.Rmd .travis_after_success.sh .travis_deploy.sh .travis_check.sh "
#git submodule  add https://github.com/rethomics/$TEMPLATE.git

for t in $template_files; do ln -vsf $TEMPLATE/$t $t;done
# travis config file cannot be symlinked 
cp $TEMPLATE/.travis.yml .travis.yml



