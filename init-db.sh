#! /bin/bash

DB=/var/local/eric/wave/db.sqlite
rm -f "$DB"
sqlite3 "$DB" < schema.sql
chmod g+w "$DB"
