#! /bin/bash

DB=/var/local/eric/hermod/db.sqlite
mkdir -p `dirname $DB`
rm -f "$DB"
sqlite3 "$DB" < schema.sql
chmod g+w "$DB"
