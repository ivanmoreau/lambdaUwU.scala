#!/bin/bash

wget https://github.com/com-lihaoyi/mill/releases/download/0.10.12/0.10.12 -O mill
chmod +x mill

rm dist/main.js
rm dist/main.js.map
./mill -i lambda.fullLinkJS

cp out/lambda/fullLinkJS.dest/main.js dist/main.js
cp out/lambda/fullLinkJS.dest/main.js.map dist/main.js.map