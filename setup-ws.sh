#! /bin/bash

cd ws/
HERE=`pwd`
for f in wave.lisp common.lisp; do
    (
	cd /var/www/html/ws/wave;
	ln -s "$HERE/$f" "$f"
    )
done
