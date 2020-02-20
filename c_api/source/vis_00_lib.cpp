
#include "utils.h"

#include "globals.h"

#include "proto2.h"
;
extern State state;
#include <AMDTPowerProfileApi.h>
#include <cassert>
#include <chrono>
#include <cstdio>
#include <fstream>
#include <iostream.h>
#include <string>
using namespace std::chrono_literals;
extern "C" {
int ProfileInitialize(int mode) { return AMDTPwrProfileInitialize(mode); }
int GetSupportedCounters_num() {
  AMDTUInt32 n = 0;
  AMDTPwrCounterDesc *desc = nullptr;
  auto res = GetSupportedCounters(&n, &desc);
  if (!((AMDT_STATUS_OK) == (res))) {
    return -1;
  };
  return n;
}
};