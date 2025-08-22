#!/bin/bash
# - run this in the cp dir to create a dictionary of strings that appear in the executable and
# thus might be useful for fuzzing...not as seeds per se, but dictionary is a different fuzzer input.
# - give it project.yaml and file to put results in.
PROGNAME=$(basename "$0")
warn()  { echo "$PROGNAME: ${@}" 1>&2; }
die()   { warn "${@}"; exit 1; }
dbug()   { test -z $DEBUG || warn "${@}"; }

# this is where this source script lives, not the cwd
thisdir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && cd -P "$( dirname "$SOURCE" )" && /bin/pwd )"

yamlfile=$1
outfile=$2



dbug Running strings on build artifacts to generate fuzzer dictionary

if [[ $(yq '.language' $yamlfile) == "java" ]]; then
    echo "Running java behavior"
    echo "starting jar unzip.."

    # Magic strings from aixcc/jazzer-examples
    echo "jazzer-traversal\njazze\njaz.Zer\njazzer\njazzer.example.com" > $outfile

    yq '.cp_sources| .[] |.artifacts' project.yaml | cut -d " " -f 2 | grep jar | while read -r JAR_FILE; do
        unzip -jp "$JAR_FILE" '*class' | strings -d  >> $outfile
    done
else
    echo "Running c behavior"
    yq '.cp_sources| .[] |.artifacts' $yamlfile|cut -d " " -f 2|xargs strings -d >> $outfile
fi

echo hello >>$outfile # in case nothing came out, so libfuzzer doesnt crash

# now we'll try to clean and format so libfuzzer doesnt die horribly in its weak dict parser

# Our solution is to turn all strings into escaped hex (compatible with
# libfuzzer dictionary format), because we wanna keep the content of these strings (ie,
# its x-evil-backdoor not xevilbackdoor)
touch /tmp/a
$thisdir/make-string-dictionary-helper.py $outfile /tmp/a

mv /tmp/a $outfile

dbug created `wc -l $outfile` dictionary entries in $outfile

exit 0  # never fail, for fear this doesnt work and kills everything
