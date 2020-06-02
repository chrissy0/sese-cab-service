#!/bin/bash
echo $PATH
/usr/bin/gprbuild --version
echo "test" > test
cat test


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
	echo "Switching directory to $1"
else
	echo "Error: Couldnt switch directory to $DIR!"
	exit 4;
fi

echo ""
echo "COMPILING PROJECT:"
echo ""

if gprbuild "$PROJECT_NAME"; then
	echo ""
	echo "Compilation sucessfull!"
else
	echo ""
	echo "Compilation failed!" -x
	exit 1
fi

echo "--------------------"
echo ""
echo "EXECUTING TESTS"
echo ""
OUTPUT_FILE="${PROJECT_NAME}_output.xml"
echo "$OUTPUT_FILE"
./"$PROJECT_NAME" > "$OUTPUT_FILE"
#xargs trims error message: Only real error, when it contains non-whitespace or linebreak character
ERROR=$(xmlstarlet sel -t -m '//FailedTests[1]' -v . -n <"$OUTPUT_FILE")
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
