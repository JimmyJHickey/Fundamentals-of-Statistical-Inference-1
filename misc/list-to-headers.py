###
# Jimmy Hickey
# 2019-09-11
# Convert comma separted list to markdown headers with whitespace
###
import sys

text = sys.argv[1]

text_array = text.split(',')

text_array = [ "# " + i.strip(' ') for i in text_array ]
print('\n****\n\n\n'.join(text_array) + "\n****\n\n")
