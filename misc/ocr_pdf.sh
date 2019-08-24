#!/bin/bash

###
# Jimmy Hickey
# 2019-08-24
# Take a PDF and make it searchable.
# Uses imagemagick and tesseract.
###

# get file name without extension
filename=$(basename -- "$1")
filename="${filename%.*}"

TEMP_TIFF=${filename}.tiff

TEMP_PDF=${filename}.temp.pdf
TEMP_FILE_NAME="${TEMP_PDF%.*}"
cp $1 ${TEMP_PDF}


# from imagemagick
# convert PDF to tiff so that it can go into tesseract
convert -density 300 ${TEMP_PDF} -depth 8 -strip -background white -alpha off ${TEMP_TIFF}

# from tesseract
# annotate and convert tiff into PDF
tesseract ${TEMP_TIFF} ${TEMP_FILE_NAME} -l eng PDF

# clean up temp files
mv ${TEMP_PDF} $1
rm ${TEMP_TIFF}
