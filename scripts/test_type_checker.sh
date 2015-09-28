# Runs compiler in non-generation mode, to exercise the early parts of the
# compiler's codepath, on the contents of the tests directory.
#
# Nik Sultana, Cambridge University Computer Lab, July 2015
#
# Example usage:
#   ./scripts/test_type_checker.sh | grep fail
#
# NOTE expects to find the compiler in the working directory.

DIR=tests/flick_code/

# NOTE tests containing "bad" in their name are filtered out. They are known
#      negative tests. Perhaps should include them and flip the result?
for FILE in `ls $DIR | grep -v bad`
do
  ./motto.byte --unexceptional -q "$DIR$FILE"
  STATUS=$?
  echo -n "${FILE} : "
  if [ ${STATUS} -eq 0 ]
  then
    echo "ok"
  else
    echo "fail"
  fi
done
