.PHONY: run
run:
	-rm image.ppm
	-rm ./smallpt-hs
	ghc smallpt.hs -O2 -o smallpt-hs -package vector -package unboxed-ref
	time ./smallpt-hs

	-rm ./image-cpp.ppm
	-rm ./a.out
	g++ smallpt-old.cpp -O2 -o a.out
	time ./a.out
	sha1sum image-cpp.ppm
	sha1sum image.ppm
