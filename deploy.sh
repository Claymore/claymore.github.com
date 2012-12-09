git checkout master
git checkout hakyll -- .gitignore
git rm --quiet --cached .gitignore
echo ".gitignore" >> .gitignore
rm -rf {assets,atom.xml,blog,favicon.png,images,index.html,javascripts,sitemap.xml,stylesheets}
cp -fR _site/* ./
NOW=`date --rfc-3339 seconds`
git add .
git commit -m "Site updated at $NOW"
rm .gitignore
git checkout hakyll
