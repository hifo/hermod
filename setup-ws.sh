#! /bin/bash

cd ws/
HERE=`pwd`
for f in hermod.lisp common.lisp; do
    (
	cd /var/www/html/ws/hermod;
	ln -s "$HERE/$f" "$f"
    )
done
