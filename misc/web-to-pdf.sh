# Take a web page and save it as a pdf with electron-pdf
# https://www.npmjs.com/package/electron-pdf

ELECTRON=/usr/local/bin/electron-pdf

WEBPAGE=$1


pdfname=$(echo ${WEBPAGE} | sed -E "s/.*com\/(.*)/\\1/")".pdf"

${ELECTRON} ${WEBPAGE} ~/Desktop/${pdfname} -e
