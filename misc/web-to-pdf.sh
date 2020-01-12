# Take a web page and save it as a pdf with electron-pdf
# https://www.npmjs.com/package/electron-pdf

ELECTRON=/usr/local/bin/electron-pdf

WEBPAGE=$1


pdfname=$(echo ${WEBPAGE} | sed -E "s/.*com\/(.*)/\\1/")".pdf"

# electron webpage path-to-pdf -e
#	-e for async save for my mathjax to load
${ELECTRON} ${WEBPAGE} ~/Desktop/${pdfname} -e
