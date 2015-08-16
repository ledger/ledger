CC = g++
CFLAGS = -g

all: smoketest regressiontest negativetest utf8readertest

smoketest:
	cd smoke_test &&  $(MAKE) $@

regressiontest:
	cd regression_tests &&  $(MAKE) $@

negativetest:
	cd negative &&  $(MAKE) $@

utf8readertest:
	cd utf8reader &&  $(MAKE) $@

clean: 
	rm smoke_test/smoketest regression_tests/regressiontest negative/negative utf8reader/utf8reader
