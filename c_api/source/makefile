CXXFLAGS=-shared -march=native -O2 \
-I../../code/src/amd/include/
libamdpowerprof.so: globals.h  proto2.h  utils.h  vis_00_lib.cpp
	g++ -o $@ vis_00_lib.cpp $(CXXFLAGS) 
