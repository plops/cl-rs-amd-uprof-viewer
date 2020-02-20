
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

  (std::cout)
      << (std::setw(10))
      << (std::chrono::high_resolution_clock::now().time_since_epoch().count())
      << (" ") << (std::this_thread::get_id()) << (" ") << (__FILE__) << (":")
      << (__LINE__) << (" ") << (__func__) << (" ")
      << ("AMDTPwrProfileInitialize_online") << (" ") << (std::endl)
      << (std::flush);
  auto mode_ = AMDT_PWR_MODE_TIMELINE_ONLINE;
  return AMDTPwrProfileInitialize(mode_);
}
int EnableCounter(int counter) {

  (std::cout)
      << (std::setw(10))
      << (std::chrono::high_resolution_clock::now().time_since_epoch().count())
      << (" ") << (std::this_thread::get_id()) << (" ") << (__FILE__) << (":")
      << (__LINE__) << (" ") << (__func__) << (" ") << ("AMDTPwrEnableCounter")
      << (" ") << (std::setw(8)) << (" counter='") << (counter) << ("'")
      << (std::endl) << (std::flush);
  return AMDTPwrEnableCounter(counter);
}
int SetTimerSamplingPeriod(int interval_ms) {

  (std::cout)
      << (std::setw(10))
      << (std::chrono::high_resolution_clock::now().time_since_epoch().count())
      << (" ") << (std::this_thread::get_id()) << (" ") << (__FILE__) << (":")
      << (__LINE__) << (" ") << (__func__) << (" ")
      << ("AMDTPwrSetTimerSamplingPeriod") << (" ") << (std::setw(8))
      << (" interval_ms='") << (interval_ms) << ("'") << (std::endl)
      << (std::flush);
  return AMDTPwrSetTimerSamplingPeriod(interval_ms);
}
int StartProfiling() {

  (std::cout)
      << (std::setw(10))
      << (std::chrono::high_resolution_clock::now().time_since_epoch().count())
      << (" ") << (std::this_thread::get_id()) << (" ") << (__FILE__) << (":")
      << (__LINE__) << (" ") << (__func__) << (" ") << ("AMDTPwrStartProfiling")
      << (" ") << (std::endl) << (std::flush);
  return AMDTPwrStartProfiling();
}
int StopProfiling() {

  (std::cout)
      << (std::setw(10))
      << (std::chrono::high_resolution_clock::now().time_since_epoch().count())
      << (" ") << (std::this_thread::get_id()) << (" ") << (__FILE__) << (":")
      << (__LINE__) << (" ") << (__func__) << (" ") << ("AMDTPwrStopProfiling")
      << (" ") << (std::endl) << (std::flush);
  return AMDTPwrStopProfiling();
}
int ProfileClose() {

  (std::cout)
      << (std::setw(10))
      << (std::chrono::high_resolution_clock::now().time_since_epoch().count())
      << (" ") << (std::this_thread::get_id()) << (" ") << (__FILE__) << (":")
      << (__LINE__) << (" ") << (__func__) << (" ") << ("AMDTPwrProfileClose")
      << (" ") << (std::endl) << (std::flush);
  return AMDTPwrProfileClose();
}
samples_pair_t ReadAllEnabledCounters() {

  (std::cout)
      << (std::setw(10))
      << (std::chrono::high_resolution_clock::now().time_since_epoch().count())
      << (" ") << (std::this_thread::get_id()) << (" ") << (__FILE__) << (":")
      << (__LINE__) << (" ") << (__func__) << (" ")
      << ("AMDTPwrReadAllEnabledCounters") << (" ") << (std::endl)
      << (std::flush);
  AMDTUInt32 n = 0;
  AMDTPwrSample *samples = nullptr;
  samples_pair_t pair = {-1, nullptr};
  auto res = AMDTPwrReadAllEnabledCounters(&n, &samples);
  if (!((AMDT_STATUS_OK) == (res))) {
    return pair;
  };
  pair.result = n;
  pair.handle = reinterpret_cast<void *>(samples);
  return pair;
}
int ReadAllEnabledCounters_PwrSample_elapsedTimeMs(void *handle, int idx) {

  (std::cout)
      << (std::setw(10))
      << (std::chrono::high_resolution_clock::now().time_since_epoch().count())
      << (" ") << (std::this_thread::get_id()) << (" ") << (__FILE__) << (":")
      << (__LINE__) << (" ") << (__func__) << (" ")
      << ("AMDTPwrReadAllEnabledCounters_PwrSample_elapsedTimeMs") << (" ")
      << (std::setw(8)) << (" handle='") << (handle) << ("'") << (std::setw(8))
      << (" idx='") << (idx) << ("'") << (std::endl) << (std::flush);
  auto samples = reinterpret_cast<AMDTPwrSample *>(handle);
  return samples[idx].m_elapsedTimeMs;
}
uint ReadAllEnabledCounters_PwrSample_recordId(void *handle, int idx) {

  (std::cout)
      << (std::setw(10))
      << (std::chrono::high_resolution_clock::now().time_since_epoch().count())
      << (" ") << (std::this_thread::get_id()) << (" ") << (__FILE__) << (":")
      << (__LINE__) << (" ") << (__func__) << (" ")
      << ("AMDTPwrReadAllEnabledCounters_PwrSample_recordId") << (" ")
      << (std::setw(8)) << (" handle='") << (handle) << ("'") << (std::setw(8))
      << (" idx='") << (idx) << ("'") << (std::endl) << (std::flush);
  auto samples = reinterpret_cast<AMDTPwrSample *>(handle);
  return samples[idx].m_recordId;
}
int ReadAllEnabledCounters_PwrSample_numOfCounter(void *handle, int idx) {

  (std::cout)
      << (std::setw(10))
      << (std::chrono::high_resolution_clock::now().time_since_epoch().count())
      << (" ") << (std::this_thread::get_id()) << (" ") << (__FILE__) << (":")
      << (__LINE__) << (" ") << (__func__) << (" ")
      << ("AMDTPwrReadAllEnabledCounters_PwrSample_numOfCounter") << (" ")
      << (std::setw(8)) << (" handle='") << (handle) << ("'") << (std::setw(8))
      << (" idx='") << (idx) << ("'") << (std::endl) << (std::flush);
  auto samples = reinterpret_cast<AMDTPwrSample *>(handle);
  return samples[idx].m_numOfCounter;
}
int ReadAllEnabledCounters_counterValues_counterID(void *handle, int idx,
                                                   int counter_idx) {

  (std::cout)
      << (std::setw(10))
      << (std::chrono::high_resolution_clock::now().time_since_epoch().count())
      << (" ") << (std::this_thread::get_id()) << (" ") << (__FILE__) << (":")
      << (__LINE__) << (" ") << (__func__) << (" ")
      << ("AMDTPwrReadAllEnabledCounters_counterValues_counterID") << (" ")
      << (std::setw(8)) << (" handle='") << (handle) << ("'") << (std::setw(8))
      << (" idx='") << (idx) << ("'") << (std::setw(8)) << (" counter_idx='")
      << (counter_idx) << ("'") << (std::endl) << (std::flush);
  auto samples = reinterpret_cast<AMDTPwrSample *>(handle);
  if ((nullptr) == (samples[idx].m_counterValues)) {
    return -1;
  };
  if (!(counter_idx < samples[idx].m_numOfCounter)) {
    return -1;
  };
  return samples[idx].m_counterValues[counter_idx].m_counterID;
}
int ReadAllEnabledCounters_counterValues_valueCnt(void *handle, int idx,
                                                  int counter_idx) {

  (std::cout)
      << (std::setw(10))
      << (std::chrono::high_resolution_clock::now().time_since_epoch().count())
      << (" ") << (std::this_thread::get_id()) << (" ") << (__FILE__) << (":")
      << (__LINE__) << (" ") << (__func__) << (" ")
      << ("AMDTPwrReadAllEnabledCounters_counterValues_valueCnt") << (" ")
      << (std::setw(8)) << (" handle='") << (handle) << ("'") << (std::setw(8))
      << (" idx='") << (idx) << ("'") << (std::setw(8)) << (" counter_idx='")
      << (counter_idx) << ("'") << (std::endl) << (std::flush);
  auto samples = reinterpret_cast<AMDTPwrSample *>(handle);
  if ((nullptr) == (samples[idx].m_counterValues)) {
    return -1;
  };
  if (!(counter_idx < samples[idx].m_numOfCounter)) {
    return -1;
  };
  return samples[idx].m_counterValues[counter_idx].m_valueCnt;
}
float ReadAllEnabledCounters_counterValues_data(void *handle, int idx,
                                                int counter_idx) {

  (std::cout)
      << (std::setw(10))
      << (std::chrono::high_resolution_clock::now().time_since_epoch().count())
      << (" ") << (std::this_thread::get_id()) << (" ") << (__FILE__) << (":")
      << (__LINE__) << (" ") << (__func__) << (" ")
      << ("AMDTPwrReadAllEnabledCounters_counterValues_data") << (" ")
      << (std::setw(8)) << (" handle='") << (handle) << ("'") << (std::setw(8))
      << (" idx='") << (idx) << ("'") << (std::setw(8)) << (" counter_idx='")
      << (counter_idx) << ("'") << (std::endl) << (std::flush);
  auto samples = reinterpret_cast<AMDTPwrSample *>(handle);
  if ((nullptr) == (samples[idx].m_counterValues)) {
    return NAN;
  };
  if (!(counter_idx < samples[idx].m_numOfCounter)) {
    return NAN;
  };
  return samples[idx].m_counterValues[counter_idx].m_data;
}
int GetSupportedCounters_num() {

  (std::cout)
      << (std::setw(10))
      << (std::chrono::high_resolution_clock::now().time_since_epoch().count())
      << (" ") << (std::this_thread::get_id()) << (" ") << (__FILE__) << (":")
      << (__LINE__) << (" ") << (__func__) << (" ")
      << ("AMDTPwrGetSupportedCounters_num") << (" ") << (std::endl)
      << (std::flush);
  AMDTUInt32 n = 0;
  AMDTPwrCounterDesc *desc = nullptr;
  auto res = AMDTPwrGetSupportedCounters(&n, &desc);
  if (!((AMDT_STATUS_OK) == (res))) {
    return -1;
  };
  return n;
}
int GetCounterDesc_counterID(int idx) {

  (std::cout)
      << (std::setw(10))
      << (std::chrono::high_resolution_clock::now().time_since_epoch().count())
      << (" ") << (std::this_thread::get_id()) << (" ") << (__FILE__) << (":")
      << (__LINE__) << (" ") << (__func__) << (" ")
      << ("AMDTPwrGetCounterDesc_counterID") << (" ") << (std::setw(8))
      << (" idx='") << (idx) << ("'") << (std::endl) << (std::flush);
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

  (std::cout)
      << (std::setw(10))
      << (std::chrono::high_resolution_clock::now().time_since_epoch().count())
      << (" ") << (std::this_thread::get_id()) << (" ") << (__FILE__) << (":")
      << (__LINE__) << (" ") << (__func__) << (" ")
      << ("AMDTPwrGetCounterDesc_deviceId") << (" ") << (std::setw(8))
      << (" idx='") << (idx) << ("'") << (std::endl) << (std::flush);
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

  (std::cout)
      << (std::setw(10))
      << (std::chrono::high_resolution_clock::now().time_since_epoch().count())
      << (" ") << (std::this_thread::get_id()) << (" ") << (__FILE__) << (":")
      << (__LINE__) << (" ") << (__func__) << (" ")
      << ("AMDTPwrGetCounterDesc_devType") << (" ") << (std::setw(8))
      << (" idx='") << (idx) << ("'") << (std::endl) << (std::flush);
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

  (std::cout)
      << (std::setw(10))
      << (std::chrono::high_resolution_clock::now().time_since_epoch().count())
      << (" ") << (std::this_thread::get_id()) << (" ") << (__FILE__) << (":")
      << (__LINE__) << (" ") << (__func__) << (" ")
      << ("AMDTPwrGetCounterDesc_devInstanceId") << (" ") << (std::setw(8))
      << (" idx='") << (idx) << ("'") << (std::endl) << (std::flush);
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

  (std::cout)
      << (std::setw(10))
      << (std::chrono::high_resolution_clock::now().time_since_epoch().count())
      << (" ") << (std::this_thread::get_id()) << (" ") << (__FILE__) << (":")
      << (__LINE__) << (" ") << (__func__) << (" ")
      << ("AMDTPwrGetCounterDesc_name") << (" ") << (std::setw(8)) << (" idx='")
      << (idx) << ("'") << (std::endl) << (std::flush);
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

  (std::cout)
      << (std::setw(10))
      << (std::chrono::high_resolution_clock::now().time_since_epoch().count())
      << (" ") << (std::this_thread::get_id()) << (" ") << (__FILE__) << (":")
      << (__LINE__) << (" ") << (__func__) << (" ")
      << ("AMDTPwrGetCounterDesc_description") << (" ") << (std::setw(8))
      << (" idx='") << (idx) << ("'") << (std::endl) << (std::flush);
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

  (std::cout)
      << (std::setw(10))
      << (std::chrono::high_resolution_clock::now().time_since_epoch().count())
      << (" ") << (std::this_thread::get_id()) << (" ") << (__FILE__) << (":")
      << (__LINE__) << (" ") << (__func__) << (" ")
      << ("AMDTPwrGetCounterDesc_category") << (" ") << (std::setw(8))
      << (" idx='") << (idx) << ("'") << (std::endl) << (std::flush);
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

  (std::cout)
      << (std::setw(10))
      << (std::chrono::high_resolution_clock::now().time_since_epoch().count())
      << (" ") << (std::this_thread::get_id()) << (" ") << (__FILE__) << (":")
      << (__LINE__) << (" ") << (__func__) << (" ")
      << ("AMDTPwrGetCounterDesc_aggregation") << (" ") << (std::setw(8))
      << (" idx='") << (idx) << ("'") << (std::endl) << (std::flush);
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

  (std::cout)
      << (std::setw(10))
      << (std::chrono::high_resolution_clock::now().time_since_epoch().count())
      << (" ") << (std::this_thread::get_id()) << (" ") << (__FILE__) << (":")
      << (__LINE__) << (" ") << (__func__) << (" ")
      << ("AMDTPwrGetCounterDesc_minValue") << (" ") << (std::setw(8))
      << (" idx='") << (idx) << ("'") << (std::endl) << (std::flush);
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

  (std::cout)
      << (std::setw(10))
      << (std::chrono::high_resolution_clock::now().time_since_epoch().count())
      << (" ") << (std::this_thread::get_id()) << (" ") << (__FILE__) << (":")
      << (__LINE__) << (" ") << (__func__) << (" ")
      << ("AMDTPwrGetCounterDesc_maxValue") << (" ") << (std::setw(8))
      << (" idx='") << (idx) << ("'") << (std::endl) << (std::flush);
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

  (std::cout)
      << (std::setw(10))
      << (std::chrono::high_resolution_clock::now().time_since_epoch().count())
      << (" ") << (std::this_thread::get_id()) << (" ") << (__FILE__) << (":")
      << (__LINE__) << (" ") << (__func__) << (" ")
      << ("AMDTPwrGetCounterDesc_units") << (" ") << (std::setw(8))
      << (" idx='") << (idx) << ("'") << (std::endl) << (std::flush);
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

  (std::cout)
      << (std::setw(10))
      << (std::chrono::high_resolution_clock::now().time_since_epoch().count())
      << (" ") << (std::this_thread::get_id()) << (" ") << (__FILE__) << (":")
      << (__LINE__) << (" ") << (__func__) << (" ")
      << ("AMDTPwrGetCounterDesc_isParentCounter") << (" ") << (std::setw(8))
      << (" idx='") << (idx) << ("'") << (std::endl) << (std::flush);
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