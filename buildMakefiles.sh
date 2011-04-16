#!/bin/bash
#===============================================================================
#
#          FILE:  buildMakefiles.sh
# 
#         USAGE:  ./buildMakefiles.sh 
# 
#   DESCRIPTION:  Using calpt as the template, copy the Makefile to 
#                 the other source directories and fix it up for that program
# 
#       OPTIONS:  ---
#  REQUIREMENTS:  ---
#          BUGS:  ---
#         NOTES:  ---
#        AUTHOR:  Paul R. DeStefano (), paul.destefano(ta)willamettealumni(dot)com.none
#       COMPANY:  
#       VERSION:  1.0
#       CREATED:  11/18/2009 08:36:28 PM PST
#      REVISION:  ---
#===============================================================================

#dirList="$(ls */Makefile | sed 's/.\/Makefile$//' )"
dirList="$(find * -maxdepth 1 -type f -name "*.F" | xargs -n 1 dirname | sort | uniq )"
echo "NOTICE: found the following subdirectories have sourcefiles: ${dirList}"

# create list of depenancies
#find . -name "*.F" | xargs egrep "^[^C].*INCLUDE.*FOR"  |sort | tr -d \' | awk '{print $1, $3}' | sed -e 's/\.F/.o/' >listOfIncludeDeps.txt
#cp calpt/Makefile Makefile.template
for dir in  ${dirList}
do
  sed -e 's/calpt/'${dir}'/g' <Makefile.template >Makefile.${dir}
  mainFileLine="mainFile = $( echo ${dir} | tr [:lower:] [:upper:] ).F"
  echo new mainFile: "${mainFileLine}"
  sed -e 's/^mainFile = .*$/'"${mainFileLine}"'/' <Makefile.${dir} >tmpfile; cp tmpfile Makefile.${dir}
  #srcFilesLine="$( (cd ${dir}; echo srcFiles = $(ls *.F ) ) )"
  #sed -e 's/^srcFiles = .*$/'"${srcFilesLine}"'/' <Makefile.${dir} >tmpfile; cp tmpfile Makefile.${dir}
  #objFilesLine="$( (cd ${dir}; echo objectFiles = $(ls *.F ) ) | sed -e 's/\.F/.o/g' )"
  #sed -e 's/^objectFiles = .*$/'"${objFilesLine}"'/' <Makefile.${dir} >tmpfile; cp tmpfile Makefile.${dir}
  #SRTsLine="$( (cd ${dir}; echo SRTs = $(ls *.FOR ) ) )"
  #sed -e 's/^SRTs = .*$/'"${SRTsLine}"'/' <Makefile.${dir} >tmpfile; cp tmpfile Makefile.${dir}
  mv Makefile.${dir} ${dir}/.
  mv ${dir}/Makefile.${dir} ${dir}/Makefile
done

rm tmpfile
