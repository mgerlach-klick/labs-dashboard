lein clean
rm target/cljsbuild/public/js/app.js
lein cljsbuild once min
cp -r resources/public/* dist/
cp target/cljsbuild/public/js/app.js dist/js/app.js
echo https://s3.amazonaws.com/labs-dashboard/index.html
echo http://bit.do/labs-dashboard
