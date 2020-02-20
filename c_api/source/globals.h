#ifndef GLOBALS_H

#define GLOBALS_H

#include <complex>
#include <condition_variable>
#include <deque>
#include <map>
#include <mutex>
#include <queue>
#include <string>
#include <thread>

#include <chrono>
struct State {
  typeof(std::chrono::high_resolution_clock::now().time_since_epoch().count())
      _start_time;
};
typedef struct State State;

#endif
