CXXFLAGS=-shared -march=native -O2 -fPIC \
-I../../code/src/amd/include/
LD_FLAGS=-lAMDPowerProfileAPI
libamdpowerprof.so: globals.h  proto2.h  utils.h  vis_00_lib.cpp
	g++ -o $@ vis_00_lib.cpp $(CXXFLAGS)  $(LD_FLAGS)
clean:
	rm libamdpowerprof.so
