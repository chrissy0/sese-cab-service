# @author Julian Hartmer and Christopher Woggon
# 
# Runs the specified ADA unit testing project and evaluates the XML output.
# Returns 0 if test successfull, otherwise 1. Used in Jenkins for CI of
# ADA Unit testing

#!/bin/sh
if [ -z "$1" ]; then
	echo "Usage: $0 GPR_FILE_WITHOUT_ENDING"
	echo "Example: $0 Ring_Buffer_Testing/test_ring_buffer"
	exit 2
fi

if test -f "$1.gpr"; then
	echo "GPR file $1.gpr found!"
else
	echo "GPR file $1.gpr not found"
	exit 3
fi

DIR=$(dirname "$1")
PROJECT_NAME=$(basename "$1")
if cd "$DIR"; then
	echo "Switching directory to $DIR"
else
	echo "Error: Couldnt switch directory to $DIR!"
	exit 4;
fi

echo ""
echo "COMPILING PROJECT $PROJECT_NAME:"
echo ""

if gprbuild "$PROJECT_NAME.gpr"; then
	echo ""
	echo "Compilation sucessfull!"
else
	echo ""
	echo "Compilation failed!"
	exit 1
fi

echo "--------------------"
echo ""
echo "EXECUTING TESTS"
echo ""
OUTPUT_FILE="${PROJECT_NAME}_output"
OUTPUT_XML="${PROJECT_NAME}_output.xml"
# remove everything until first xml tag
# this is a hacky way to remove all Put_Line's in the tests

echo "$OUTPUT_FILE"
./"$PROJECT_NAME" > "$OUTPUT_FILE"
# trim everything up until <TestRun>
awk '/<TestRun>/,0' $OUTPUT_FILE > $OUTPUT_XML
ERROR=$(xmlstarlet sel -t -m '//FailedTests[1]' -v . -n <"$OUTPUT_XML")
ERROR_TRIMMED=$(echo "$ERROR" | xargs)
echo "--------------------"
echo ""
if [ -z "$ERROR_TRIMMED" ]; then
	echo "Test successfull!"
	exit 0
else
	echo "Test failed!"
	echo "Test report:"
	echo "-------------"
	cat "$OUTPUT_FILE"
	echo "------------"
	exit 1
fi
