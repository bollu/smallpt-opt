.PHONY: run runhs runcpp runhs-llvm runcpp-reference runcpp-notrick erand48

erand48:
	g++ erand48.cpp -o erand48.out
	./erand48.out
	sha1sum ./erand48.txt > erand48.sha
	ghc erand48.hs -package unboxed-ref -o erand48.out
	./erand48.out > erand48.txt
	sha1sum -c erand48.sha

diff: runcpp-reference runcpp-notrick

runcpp-reference:
	-rm ./image-cpp.ppm
	-rm ./a.out
	g++ smallpt-old.cpp -O2 -fsanitize=address -fsanitize=undefined -static-libasan \
		 -o a.out
	time ./a.out
	sha1sum image-cpp.ppm > image-cpp.sha

runcpp-notrick:
	-rm ./image-cpp.ppm
	-rm ./b.out
	g++ smallpt-old-notrick.cpp -fsanitize=address -fsanitize=undefined -O2 \
		-static-libasan -o b.out
	time ./b.out
	sha1sum -c image-cpp.sha # check the sha1sum of image-cpp.sha


runhs:
	-rm image.ppm
	-rm ./smallpt-hs
	# https://llvm.org/docs/CommandGuide/llc.html
	ghc smallpt.hs -O2 -o smallpt-hs -package vector -package unboxed-ref -ddump-cmm -ddump-to-file

	/usr/bin/time ./smallpt-hs

