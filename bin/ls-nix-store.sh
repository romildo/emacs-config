#! /bin/sh

ls_output=$(ls "$@")
nl='
'
echo "${ls_output%%"$nl"*}" # The total... line
ls_output=${ls_output#*"$nl"}
# Isolate the //DIRED stuff, which must go last
case "$ls_output" in
  *"$nl//DIRED"*)
    dired_stuff="//DIRED${ls_output##*"$nl//DIRED"}$nl"
    ls_output="${ls_output%"$nl//DIRED"*}";;
  *) dired_stuff=;;
esac
# Now sort, ignoring the first 8 fields (metadata) and the first 33 characters of the 9th field
printf '%s\n' "$ls_output" | sort -k 9.34
printf '%s' "$dired_stuff"
