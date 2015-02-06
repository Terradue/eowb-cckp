#!/bin/bash
# Project: ${project.name}
# Author: $Author: fbrito $ (Terradue Srl)
# Last update: ${doc.timestamp}:
# Element: ${project.name}
# Context: ${project.artifactId}
# Version: ${project.version} (${implementation.build})
# Description: ${project.description}
#
# This document is the property of Terradue and contains information directly
# resulting from knowledge and experience of Terradue.
# Any changes to this code is forbidden without written consent from Terradue Srl
#
# Contact: info@terradue.com
# source the ciop functions (e.g. ciop-log)

source ${ciop_job_include}

# define the exit codes
SUCCESS=0
ERR_INVALID_DATE=1

# add a trap to exit gracefully
function cleanExit ()
{
	local retval=$?
	local msg=""

	case "$retval" in
		$SUCCESS) msg="Processing successfully concluded";;
		$ERR_INVALID_DATE) msg="Date format is invalid";;
		*) msg="Unknown error";;
	esac

	[ "$retval" != "0" ] && ciop-log "ERROR" "Error $retval - $msg, processing aborted" || ciop-log "INFO" "$msg"
	exit $retval
}
trap cleanExit EXIT

startdate=`ciop-getparam startdate`
stopdate=`ciop-getparam stopdate`
joborder="/tmp/`uuidgen`.joborder"
#mycountries=`ciop-getparam ISO3166`
mycountries="/application/data/countries.txt"
nolines=$( cat ${mycountries} | wc -l )
counter=0

ciop-log "INFO" "processing input [${startdate}] [${stopdate}] [${joborder}] [${nolines}] [${mycountries}]"

cat ${mycountries}

while [ "`python -c "from datetime import date; print (date($( echo "${stopdate}" | sed 's#-0\(.\)-#-\1-#1' | sed 's#-0\(.\)$#-\1#1' | tr '-' ','  ))-date($( echo "${startdate}" | sed 's#-0\(.\)-#-\1-#1' | sed 's#-0\(.\)$#-\1#1' | tr '-' ',' ))).days"`" -ge 0 ]
do
	ciop-log "DEBUG" "working with date: ${startdate}"
	for mycountry in `cat ${mycountries}`
	do
		echo "${mycountry};${startdate}" >> ${joborder}
		counter=$(( $counter + 1 ))
	done
	startdate=`date --date="$startdate 1 day" "+%Y-%m-%d"`
done

ciop-log "DEBUG" "wrote $( cat ${joborder} | wc -l  ) lines"

#let's publish the joborder file
cat
[ -e "${joborder}" ] && cat ${joborder} | ciop-publish -s 

#clean up and finish
exit $SUCCESS
