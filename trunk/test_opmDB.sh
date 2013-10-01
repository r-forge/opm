#!/bin/sh


################################################################################
#
# INSTALLATIONS NEEDED
# ====================
#
# sudo apt-get install mysql-server
# sudo apt-get install libmysqlclient-dev ## for compiling RMySQL
#
# sudo apt-get install sqlite3 ## earlier versions do not work
#   ## RSQLite brings its own SQLite source code
#
# SET UP DATABASE FOR MYSQL
# =========================
# ## in the command line, enter (without the '$'):
# $ mysql -u root -p
# ## in the mysl prompt, enter (without 'mysql> ' and after replacing $USER
# ## with your username:
# mysql> CREATE DATABASE IF NOT EXISTS pmdata;
# mysql> GRANT USAGE ON pmdata.* TO $USER@localhost;
# mysql> GRANT ALL PRIVILEGES ON pmdata.* TO $USER@localhost;
#
################################################################################


set -eu


################################################################################


test_sql_with_sqlite3()
{
  local dbname=$1
  shift
  local infile
  for infile; do
    sqlite3 -bail -batch "$dbname" < "$infile" > /dev/null || return 1
  done
}


test_sql_with_postgresql()
{
  local dbname=$1
  shift
  local infile
  for infile; do
    psql -q -1 -o /dev/null -d "$dbname" -f "$infile" || return 1
  done
}


test_sql_with_mysql()
{
  local dbname=$1
  shift
  local infile
  for infile; do
    mysql --show-warnings -B -s "$dbname" < "$infile" > /dev/null || return 1
  done
}


bla()
{
  local result
  [ "$1" -gt 0 ] && result=FAILED || result=SUCCEEDED
  [ $# -gt 1 ] && echo "* $2 TESTS: $result" || echo "*** TESTS: $result ***"
  echo
}


################################################################################


errs=0
outcome=0
sqlite3_dbname=pmdata
postgreqsql_dbname=pmdata
mysql_dbname=pmdata
[ $# -eq 0 ] && set `find opmDB_in -iname '*.sql' -exec ls \{\} +`


################################################################################


test_sql_with_sqlite3 "$sqlite3_dbname" "$@" && outcome=0 || outcome=1
bla $outcome SQLITE3
errs=$((errs + outcome))

test_sql_with_postgresql "$postgreqsql_dbname" "$@" && outcome=0 || outcome=1
bla $outcome POSTGRESQL
errs=$((errs + outcome))

test_sql_with_mysql "$mysql_dbname" "$@" && outcome=0 || outcome=1
bla $outcome MYSQL
errs=$((errs + outcome))

bla $errs
exit $errs




