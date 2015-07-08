# Runs compiler in non-generation mode, to exercise the early parts of the
# compiler's codepath, on the contents of the tests directory.
#
# Nik Sultana, Cambridge University Computer Lab, July 2015
#
# NOTE expects to find the compiler in the working directory.

DIR=tests/

# NOTE tests containing "bad" in their name are filtered out. They are known
#      negative tests. Perhaps should include them and flip the result?
for FILE in `ls $DIR | grep -v bad`
do
  ./otto.byte --unexceptional -q "$DIR$FILE"
  STATUS=$?
  echo -n "${FILE} : "
  if [ ${STATUS} -eq 0 ]
  then
    echo "ok"
  else
    echo "fail"
  fi
done
