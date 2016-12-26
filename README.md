lein clean
lein cljsbuild once min
cp -r resources/public/* dist/
cp target/cljsbuild/public/js/app.js dist/js/app.js
https://s3.amazonaws.com/labs-dashboard/index.html
http://bit.do/labs-dashboard
