.PHONY: test-guile test-gauche test-kawa test-chibi test-chicken

test-guile:
	guile -L . --r7rs dictionaries-test.scm

test-gauche:
	gosh -I . dictionaries-test.scm

test-kawa:
	cp dictionaries.sld dictionaries.scm
	kawa dictionaries-test.scm
	rm dictionaries.scm

test-chibi:
	chibi-scheme dictionaries-test.scm

test-chicken:
	csc -R r7rs -X r7rs -sJ -o dictionaries.so dictionaries.sld
	csi -I . -R r7rs -s dictionaries-test.scm
	rm dictionaries.so
	rm dictionaries.import.scm
