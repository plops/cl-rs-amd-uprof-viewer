
#include "utils.h"

#include "globals.h"

;
extern State state;
#include <AMDTPowerProfileApi.h>
#include <cassert>
#include <chrono>
#include <cstdio>
#include <fstream>
#include <string>
using namespace std::chrono_literals;
extern "C" {
struct samples_pair_t {
  int result;
  void *handle;
};
typedef struct samples_pair_t samples_pair_t;
int ProfileInitialize_online() {
  auto mode_ = AMDT_PWR_MODE_TIMELINE_ONLINE;
  return AMDTPwrProfileInitialize(mode_);
}
int EnableCounter(int counter) { return AMDTPwrEnableCounter(counter); }
int SetTimerSamplingPeriod(int interval_ms) {
  return AMDTPwrSetTimerSamplingPeriod(interval_ms);
}
int StartProfiling() { return AMDTPwrStartProfiling(); }
int StopProfiling() { return AMDTPwrStopProfiling(); }
int ProfileClose() { return AMDTPwrProfileClose(); }
samples_pair_t ReadAllEnabledCounters() {
  AMDTUInt32 n = 0;
  AMDTPwrSample *desc = nullptr;
  samples_pair_t pair = {-1, nullptr};
  auto res = AMDTPwrReadAllEnabledCounters(&n, &desc);
  if (!((AMDT_STATUS_OK) == (res))) {
    return pair;
  };
  pair.result = n;
  pair.handle = reinterpret_cast<void *>(desc);
  return pair;
}
int GetSupportedCounters_num() {
  AMDTUInt32 n = 0;
  AMDTPwrCounterDesc *desc = nullptr;
  auto res = AMDTPwrGetSupportedCounters(&n, &desc);
  if (!((AMDT_STATUS_OK) == (res))) {
    return -1;
  };
  return n;
}
int GetCounterDesc_counterID(int idx) {
  AMDTUInt32 n = 0;
  AMDTPwrCounterDesc *desc = nullptr;
  auto res = AMDTPwrGetSupportedCounters(&n, &desc);
  if (!((AMDT_STATUS_OK) == (res))) {
    return -1;
  };
  if (!(idx < n)) {
    return -1;
  };
  return desc[idx].m_counterID;
}
int GetCounterDesc_deviceId(int idx) {
  AMDTUInt32 n = 0;
  AMDTPwrCounterDesc *desc = nullptr;
  auto res = AMDTPwrGetSupportedCounters(&n, &desc);
  if (!((AMDT_STATUS_OK) == (res))) {
    return -1;
  };
  if (!(idx < n)) {
    return -1;
  };
  return desc[idx].m_deviceId;
}
int GetCounterDesc_devType(int idx) {
  AMDTUInt32 n = 0;
  AMDTPwrCounterDesc *desc = nullptr;
  auto res = AMDTPwrGetSupportedCounters(&n, &desc);
  if (!((AMDT_STATUS_OK) == (res))) {
    return -1;
  };
  if (!(idx < n)) {
    return -1;
  };
  return desc[idx].m_devType;
}
int GetCounterDesc_devInstanceId(int idx) {
  AMDTUInt32 n = 0;
  AMDTPwrCounterDesc *desc = nullptr;
  auto res = AMDTPwrGetSupportedCounters(&n, &desc);
  if (!((AMDT_STATUS_OK) == (res))) {
    return -1;
  };
  if (!(idx < n)) {
    return -1;
  };
  return desc[idx].m_devInstanceId;
}
char *GetCounterDesc_name(int idx) {
  AMDTUInt32 n = 0;
  AMDTPwrCounterDesc *desc = nullptr;
  auto res = AMDTPwrGetSupportedCounters(&n, &desc);
  if (!((AMDT_STATUS_OK) == (res))) {
    return nullptr;
  };
  if (!(idx < n)) {
    return nullptr;
  };
  return desc[idx].m_name;
}
char *GetCounterDesc_description(int idx) {
  AMDTUInt32 n = 0;
  AMDTPwrCounterDesc *desc = nullptr;
  auto res = AMDTPwrGetSupportedCounters(&n, &desc);
  if (!((AMDT_STATUS_OK) == (res))) {
    return nullptr;
  };
  if (!(idx < n)) {
    return nullptr;
  };
  return desc[idx].m_description;
}
int GetCounterDesc_category(int idx) {
  AMDTUInt32 n = 0;
  AMDTPwrCounterDesc *desc = nullptr;
  auto res = AMDTPwrGetSupportedCounters(&n, &desc);
  if (!((AMDT_STATUS_OK) == (res))) {
    return -1;
  };
  if (!(idx < n)) {
    return -1;
  };
  return desc[idx].m_category;
}
int GetCounterDesc_aggregation(int idx) {
  AMDTUInt32 n = 0;
  AMDTPwrCounterDesc *desc = nullptr;
  auto res = AMDTPwrGetSupportedCounters(&n, &desc);
  if (!((AMDT_STATUS_OK) == (res))) {
    return -1;
  };
  if (!(idx < n)) {
    return -1;
  };
  return desc[idx].m_aggregation;
}
double GetCounterDesc_minValue(int idx) {
  AMDTUInt32 n = 0;
  AMDTPwrCounterDesc *desc = nullptr;
  auto res = AMDTPwrGetSupportedCounters(&n, &desc);
  if (!((AMDT_STATUS_OK) == (res))) {
    return NAN;
  };
  if (!(idx < n)) {
    return NAN;
  };
  return desc[idx].m_minValue;
}
double GetCounterDesc_maxValue(int idx) {
  AMDTUInt32 n = 0;
  AMDTPwrCounterDesc *desc = nullptr;
  auto res = AMDTPwrGetSupportedCounters(&n, &desc);
  if (!((AMDT_STATUS_OK) == (res))) {
    return NAN;
  };
  if (!(idx < n)) {
    return NAN;
  };
  return desc[idx].m_maxValue;
}
int GetCounterDesc_units(int idx) {
  AMDTUInt32 n = 0;
  AMDTPwrCounterDesc *desc = nullptr;
  auto res = AMDTPwrGetSupportedCounters(&n, &desc);
  if (!((AMDT_STATUS_OK) == (res))) {
    return -1;
  };
  if (!(idx < n)) {
    return -1;
  };
  return desc[idx].m_units;
}
int GetCounterDesc_isParentCounter(int idx) {
  AMDTUInt32 n = 0;
  AMDTPwrCounterDesc *desc = nullptr;
  auto res = AMDTPwrGetSupportedCounters(&n, &desc);
  if (!((AMDT_STATUS_OK) == (res))) {
    return -1;
  };
  if (!(idx < n)) {
    return -1;
  };
  return desc[idx].m_isParentCounter;
}
};