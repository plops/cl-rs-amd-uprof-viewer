
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
  auto res = AMDTPwrProfileInitialize(mode_);

  (std::cout)
      << (std::setw(10))
      << (std::chrono::high_resolution_clock::now().time_since_epoch().count())
      << (" ") << (std::this_thread::get_id()) << (" ") << (__FILE__) << (":")
      << (__LINE__) << (" ") << (__func__) << (" ") << ("init") << (" ")
      << (std::setw(8)) << (" res='") << (res) << ("'") << (std::endl)
      << (std::flush);
  return res;
}
int EnableCounter(int counter) {
  auto res = AMDTPwrEnableCounter(counter);

  (std::cout)
      << (std::setw(10))
      << (std::chrono::high_resolution_clock::now().time_since_epoch().count())
      << (" ") << (std::this_thread::get_id()) << (" ") << (__FILE__) << (":")
      << (__LINE__) << (" ") << (__func__) << (" ") << ("AMDTPwrEnableCounter")
      << (" ") << (std::setw(8)) << (" res='") << (res) << ("'")
      << (std::setw(8)) << (" counter='") << (counter) << ("'") << (std::endl)
      << (std::flush);
  return res;
}
int SetTimerSamplingPeriod(int interval_ms) {
  auto res = AMDTPwrSetTimerSamplingPeriod(interval_ms);

  (std::cout)
      << (std::setw(10))
      << (std::chrono::high_resolution_clock::now().time_since_epoch().count())
      << (" ") << (std::this_thread::get_id()) << (" ") << (__FILE__) << (":")
      << (__LINE__) << (" ") << (__func__) << (" ")
      << ("AMDTPwrSetTimerSamplingPeriod") << (" ") << (std::setw(8))
      << (" res='") << (res) << ("'") << (std::setw(8)) << (" interval_ms='")
      << (interval_ms) << ("'") << (std::endl) << (std::flush);
  return res;
}
int StartProfiling() {
  auto res = AMDTPwrStartProfiling();

  (std::cout)
      << (std::setw(10))
      << (std::chrono::high_resolution_clock::now().time_since_epoch().count())
      << (" ") << (std::this_thread::get_id()) << (" ") << (__FILE__) << (":")
      << (__LINE__) << (" ") << (__func__) << (" ") << ("AMDTPwrStartProfiling")
      << (" ") << (std::setw(8)) << (" res='") << (res) << ("'") << (std::endl)
      << (std::flush);
  return res;
}
int StopProfiling() {
  auto res = AMDTPwrStopProfiling();

  (std::cout)
      << (std::setw(10))
      << (std::chrono::high_resolution_clock::now().time_since_epoch().count())
      << (" ") << (std::this_thread::get_id()) << (" ") << (__FILE__) << (":")
      << (__LINE__) << (" ") << (__func__) << (" ") << ("AMDTPwrStopProfiling")
      << (" ") << (std::setw(8)) << (" res='") << (res) << ("'") << (std::endl)
      << (std::flush);
  return res;
}
int ProfileClose() {
  auto res = AMDTPwrProfileClose();

  (std::cout)
      << (std::setw(10))
      << (std::chrono::high_resolution_clock::now().time_since_epoch().count())
      << (" ") << (std::this_thread::get_id()) << (" ") << (__FILE__) << (":")
      << (__LINE__) << (" ") << (__func__) << (" ") << ("AMDTPwrProfileClose")
      << (" ") << (std::setw(8)) << (" res='") << (res) << ("'") << (std::endl)
      << (std::flush);
  return res;
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
    switch (res) {
    case AMDT_STATUS_OK: {

      (std::cout) << (std::setw(10))
                  << (std::chrono::high_resolution_clock::now()
                          .time_since_epoch()
                          .count())
                  << (" ") << (std::this_thread::get_id()) << (" ")
                  << (__FILE__) << (":") << (__LINE__) << (" ") << (__func__)
                  << (" ") << ("AMDT_STATUS_OK") << (" ") << (std::endl)
                  << (std::flush);
      break;
    }
    case AMDT_ERROR_INVALIDARG: {

      (std::cout) << (std::setw(10))
                  << (std::chrono::high_resolution_clock::now()
                          .time_since_epoch()
                          .count())
                  << (" ") << (std::this_thread::get_id()) << (" ")
                  << (__FILE__) << (":") << (__LINE__) << (" ") << (__func__)
                  << (" ") << ("AMDT_ERROR_INVALIDARG") << (" ") << (std::endl)
                  << (std::flush);
      break;
    }
    case AMDT_ERROR_DRIVER_UNINITIALIZED: {

      (std::cout) << (std::setw(10))
                  << (std::chrono::high_resolution_clock::now()
                          .time_since_epoch()
                          .count())
                  << (" ") << (std::this_thread::get_id()) << (" ")
                  << (__FILE__) << (":") << (__LINE__) << (" ") << (__func__)
                  << (" ") << ("AMDT_ERROR_DRIVER_UNINITIALIZED") << (" ")
                  << (std::endl) << (std::flush);
      break;
    }
    case AMDT_ERROR_PROFILE_NOT_STARTED: {

      (std::cout) << (std::setw(10))
                  << (std::chrono::high_resolution_clock::now()
                          .time_since_epoch()
                          .count())
                  << (" ") << (std::this_thread::get_id()) << (" ")
                  << (__FILE__) << (":") << (__LINE__) << (" ") << (__func__)
                  << (" ") << ("AMDT_ERROR_PROFILE_NOT_STARTED") << (" ")
                  << (std::endl) << (std::flush);
      break;
    }
    case AMDT_ERROR_PROFILE_DATA_NOT_AVAILABLE: {

      (std::cout) << (std::setw(10))
                  << (std::chrono::high_resolution_clock::now()
                          .time_since_epoch()
                          .count())
                  << (" ") << (std::this_thread::get_id()) << (" ")
                  << (__FILE__) << (":") << (__LINE__) << (" ") << (__func__)
                  << (" ") << ("AMDT_ERROR_PROFILE_DATA_NOT_AVAILABLE") << (" ")
                  << (std::endl) << (std::flush);
      break;
    }
    case AMDT_ERROR_OUTOFMEMORY: {

      (std::cout) << (std::setw(10))
                  << (std::chrono::high_resolution_clock::now()
                          .time_since_epoch()
                          .count())
                  << (" ") << (std::this_thread::get_id()) << (" ")
                  << (__FILE__) << (":") << (__LINE__) << (" ") << (__func__)
                  << (" ") << ("AMDT_ERROR_OUTOFMEMORY") << (" ") << (std::endl)
                  << (std::flush);
      break;
    }
    case AMDT_ERROR_SMU_ACCESS_FAILED: {

      (std::cout) << (std::setw(10))
                  << (std::chrono::high_resolution_clock::now()
                          .time_since_epoch()
                          .count())
                  << (" ") << (std::this_thread::get_id()) << (" ")
                  << (__FILE__) << (":") << (__LINE__) << (" ") << (__func__)
                  << (" ") << ("AMDT_ERROR_SMU_ACCESS_FAILED") << (" ")
                  << (std::endl) << (std::flush);
      break;
    }
    case AMDT_ERROR_FAIL: {

      (std::cout) << (std::setw(10))
                  << (std::chrono::high_resolution_clock::now()
                          .time_since_epoch()
                          .count())
                  << (" ") << (std::this_thread::get_id()) << (" ")
                  << (__FILE__) << (":") << (__LINE__) << (" ") << (__func__)
                  << (" ") << ("AMDT_ERROR_FAIL") << (" ") << (std::endl)
                  << (std::flush);
      break;
    }
    }
    return pair;
  };
  pair.result = n;
  pair.handle = reinterpret_cast<void *>(samples);

  (std::cout)
      << (std::setw(10))
      << (std::chrono::high_resolution_clock::now().time_since_epoch().count())
      << (" ") << (std::this_thread::get_id()) << (" ") << (__FILE__) << (":")
      << (__LINE__) << (" ") << (__func__) << (" ") << ("") << (" ")
      << (std::setw(8)) << (" pair.result='") << (pair.result) << ("'")
      << (std::setw(8)) << (" pair.handle='") << (pair.handle) << ("'")
      << (std::endl) << (std::flush);
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
  auto res = samples[idx].m_elapsedTimeMs;

  (std::cout)
      << (std::setw(10))
      << (std::chrono::high_resolution_clock::now().time_since_epoch().count())
      << (" ") << (std::this_thread::get_id()) << (" ") << (__FILE__) << (":")
      << (__LINE__) << (" ") << (__func__) << (" ") << ("") << (" ")
      << (std::setw(8)) << (" idx='") << (idx) << ("'") << (std::setw(8))
      << (" res='") << (res) << ("'") << (std::endl) << (std::flush);
  return res;
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
  auto res = samples[idx].m_recordId;

  (std::cout)
      << (std::setw(10))
      << (std::chrono::high_resolution_clock::now().time_since_epoch().count())
      << (" ") << (std::this_thread::get_id()) << (" ") << (__FILE__) << (":")
      << (__LINE__) << (" ") << (__func__) << (" ") << ("") << (" ")
      << (std::setw(8)) << (" idx='") << (idx) << ("'") << (std::setw(8))
      << (" res='") << (res) << ("'") << (std::endl) << (std::flush);
  return res;
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
  auto res = samples[idx].m_numOfCounter;

  (std::cout)
      << (std::setw(10))
      << (std::chrono::high_resolution_clock::now().time_since_epoch().count())
      << (" ") << (std::this_thread::get_id()) << (" ") << (__FILE__) << (":")
      << (__LINE__) << (" ") << (__func__) << (" ") << ("") << (" ")
      << (std::setw(8)) << (" idx='") << (idx) << ("'") << (std::setw(8))
      << (" res='") << (res) << ("'") << (std::endl) << (std::flush);
  return res;
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

    (std::cout) << (std::setw(10))
                << (std::chrono::high_resolution_clock::now()
                        .time_since_epoch()
                        .count())
                << (" ") << (std::this_thread::get_id()) << (" ") << (__FILE__)
                << (":") << (__LINE__) << (" ") << (__func__) << (" ")
                << ("fail nullptr") << (" ") << (std::setw(8))
                << (" samples[idx].m_counterValues='")
                << (samples[idx].m_counterValues) << ("'") << (std::endl)
                << (std::flush);
    return NAN;
  };
  if (!(counter_idx < samples[idx].m_numOfCounter)) {

    (std::cout) << (std::setw(10))
                << (std::chrono::high_resolution_clock::now()
                        .time_since_epoch()
                        .count())
                << (" ") << (std::this_thread::get_id()) << (" ") << (__FILE__)
                << (":") << (__LINE__) << (" ") << (__func__) << (" ")
                << ("out of bounds") << (" ") << (std::setw(8))
                << (" counter_idx='") << (counter_idx) << ("'")
                << (std::setw(8)) << (" samples[idx].m_numOfCounter='")
                << (samples[idx].m_numOfCounter) << ("'") << (std::endl)
                << (std::flush);
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

    (std::cout) << (std::setw(10))
                << (std::chrono::high_resolution_clock::now()
                        .time_since_epoch()
                        .count())
                << (" ") << (std::this_thread::get_id()) << (" ") << (__FILE__)
                << (":") << (__LINE__) << (" ") << (__func__) << (" ")
                << ("fail") << (" ") << (std::setw(8)) << (" res='") << (res)
                << ("'") << (std::endl) << (std::flush);
    return -1;
  };

  (std::cout)
      << (std::setw(10))
      << (std::chrono::high_resolution_clock::now().time_since_epoch().count())
      << (" ") << (std::this_thread::get_id()) << (" ") << (__FILE__) << (":")
      << (__LINE__) << (" ") << (__func__) << (" ") << ("") << (" ")
      << (std::setw(8)) << (" n='") << (n) << ("'") << (std::setw(8))
      << (" desc='") << (desc) << ("'") << (std::endl) << (std::flush);
  auto p = desc;
  for (int i = 0; i < n; (i) += (1)) {
    if (!((nullptr) == (p))) {
      auto counterID = p->m_counterID;
      auto deviceId = p->m_deviceId;
      auto devType = p->m_devType;
      auto devInstanceId = p->m_devInstanceId;
      auto name = p->m_name;
      auto description = p->m_description;
      auto category = p->m_category;
      auto aggregation = p->m_aggregation;
      auto minValue = p->m_minValue;
      auto maxValue = p->m_maxValue;
      auto units = p->m_units;
      auto isParentCounter = p->m_isParentCounter;

      (std::cout) << (std::setw(10))
                  << (std::chrono::high_resolution_clock::now()
                          .time_since_epoch()
                          .count())
                  << (" ") << (std::this_thread::get_id()) << (" ")
                  << (__FILE__) << (":") << (__LINE__) << (" ") << (__func__)
                  << (" ") << ("") << (" ") << (std::setw(8))
                  << (" counterID='") << (counterID) << ("'") << (std::setw(8))
                  << (" deviceId='") << (deviceId) << ("'") << (std::setw(8))
                  << (" devType='") << (devType) << ("'") << (std::setw(8))
                  << (" devInstanceId='") << (devInstanceId) << ("'")
                  << (std::setw(8)) << (" name='") << (name) << ("'")
                  << (std::setw(8)) << (" description='") << (description)
                  << ("'") << (std::setw(8)) << (" category='") << (category)
                  << ("'") << (std::setw(8)) << (" aggregation='")
                  << (aggregation) << ("'") << (std::setw(8)) << (" minValue='")
                  << (minValue) << ("'") << (std::setw(8)) << (" maxValue='")
                  << (maxValue) << ("'") << (std::setw(8)) << (" units='")
                  << (units) << ("'") << (std::setw(8))
                  << (" isParentCounter='") << (isParentCounter) << ("'")
                  << (std::endl) << (std::flush);
    };
    (p)++;
  };
  return n;
}
int EnableAllCounters() {

  (std::cout)
      << (std::setw(10))
      << (std::chrono::high_resolution_clock::now().time_since_epoch().count())
      << (" ") << (std::this_thread::get_id()) << (" ") << (__FILE__) << (":")
      << (__LINE__) << (" ") << (__func__) << (" ")
      << ("AMDTPwrEnableAllCounters") << (" ") << (std::endl) << (std::flush);
  AMDTUInt32 n = 0;
  AMDTPwrCounterDesc *desc = nullptr;
  auto res = AMDTPwrGetSupportedCounters(&n, &desc);
  if (!((AMDT_STATUS_OK) == (res))) {

    (std::cout) << (std::setw(10))
                << (std::chrono::high_resolution_clock::now()
                        .time_since_epoch()
                        .count())
                << (" ") << (std::this_thread::get_id()) << (" ") << (__FILE__)
                << (":") << (__LINE__) << (" ") << (__func__) << (" ")
                << ("fail") << (" ") << (std::setw(8)) << (" res='") << (res)
                << ("'") << (std::endl) << (std::flush);
    return -1;
  };

  (std::cout)
      << (std::setw(10))
      << (std::chrono::high_resolution_clock::now().time_since_epoch().count())
      << (" ") << (std::this_thread::get_id()) << (" ") << (__FILE__) << (":")
      << (__LINE__) << (" ") << (__func__) << (" ") << ("") << (" ")
      << (std::setw(8)) << (" n='") << (n) << ("'") << (std::setw(8))
      << (" desc='") << (desc) << ("'") << (std::endl) << (std::flush);
  auto p = desc;
  for (int i = 0; i < n; (i) += (1)) {
    if (!((nullptr) == (p))) {
      auto res = AMDTPwrEnableCounter(p->m_counterID);
      if (!((AMDT_STATUS_OK) == (res))) {

        (std::cout) << (std::setw(10))
                    << (std::chrono::high_resolution_clock::now()
                            .time_since_epoch()
                            .count())
                    << (" ") << (std::this_thread::get_id()) << (" ")
                    << (__FILE__) << (":") << (__LINE__) << (" ") << (__func__)
                    << (" ") << ("fail enable") << (" ") << (std::setw(8))
                    << (" res='") << (res) << ("'") << (std::setw(8))
                    << (" i='") << (i) << ("'") << (std::setw(8))
                    << (" p->m_counterID='") << (p->m_counterID) << ("'")
                    << (std::setw(8)) << (" n='") << (n) << ("'")
                    << (std::setw(8)) << (" desc='") << (desc) << ("'")
                    << (std::endl) << (std::flush);
      };
    };
    (p)++;
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

    (std::cout) << (std::setw(10))
                << (std::chrono::high_resolution_clock::now()
                        .time_since_epoch()
                        .count())
                << (" ") << (std::this_thread::get_id()) << (" ") << (__FILE__)
                << (":") << (__LINE__) << (" ") << (__func__) << (" ")
                << ("fail") << (" ") << (std::setw(8)) << (" res='") << (res)
                << ("'") << (std::endl) << (std::flush);
    return -1;
  };
  if (!(idx < n)) {

    (std::cout) << (std::setw(10))
                << (std::chrono::high_resolution_clock::now()
                        .time_since_epoch()
                        .count())
                << (" ") << (std::this_thread::get_id()) << (" ") << (__FILE__)
                << (":") << (__LINE__) << (" ") << (__func__) << (" ")
                << ("out of bounds") << (" ") << (std::setw(8)) << (" idx='")
                << (idx) << ("'") << (std::setw(8)) << (" n='") << (n) << ("'")
                << (std::endl) << (std::flush);
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

    (std::cout) << (std::setw(10))
                << (std::chrono::high_resolution_clock::now()
                        .time_since_epoch()
                        .count())
                << (" ") << (std::this_thread::get_id()) << (" ") << (__FILE__)
                << (":") << (__LINE__) << (" ") << (__func__) << (" ")
                << ("fail") << (" ") << (std::setw(8)) << (" res='") << (res)
                << ("'") << (std::endl) << (std::flush);
    return -1;
  };
  if (!(idx < n)) {

    (std::cout) << (std::setw(10))
                << (std::chrono::high_resolution_clock::now()
                        .time_since_epoch()
                        .count())
                << (" ") << (std::this_thread::get_id()) << (" ") << (__FILE__)
                << (":") << (__LINE__) << (" ") << (__func__) << (" ")
                << ("out of bounds") << (" ") << (std::setw(8)) << (" idx='")
                << (idx) << ("'") << (std::setw(8)) << (" n='") << (n) << ("'")
                << (std::endl) << (std::flush);
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

    (std::cout) << (std::setw(10))
                << (std::chrono::high_resolution_clock::now()
                        .time_since_epoch()
                        .count())
                << (" ") << (std::this_thread::get_id()) << (" ") << (__FILE__)
                << (":") << (__LINE__) << (" ") << (__func__) << (" ")
                << ("fail") << (" ") << (std::setw(8)) << (" res='") << (res)
                << ("'") << (std::endl) << (std::flush);
    return -1;
  };
  if (!(idx < n)) {

    (std::cout) << (std::setw(10))
                << (std::chrono::high_resolution_clock::now()
                        .time_since_epoch()
                        .count())
                << (" ") << (std::this_thread::get_id()) << (" ") << (__FILE__)
                << (":") << (__LINE__) << (" ") << (__func__) << (" ")
                << ("out of bounds") << (" ") << (std::setw(8)) << (" idx='")
                << (idx) << ("'") << (std::setw(8)) << (" n='") << (n) << ("'")
                << (std::endl) << (std::flush);
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

    (std::cout) << (std::setw(10))
                << (std::chrono::high_resolution_clock::now()
                        .time_since_epoch()
                        .count())
                << (" ") << (std::this_thread::get_id()) << (" ") << (__FILE__)
                << (":") << (__LINE__) << (" ") << (__func__) << (" ")
                << ("fail") << (" ") << (std::setw(8)) << (" res='") << (res)
                << ("'") << (std::endl) << (std::flush);
    return -1;
  };
  if (!(idx < n)) {

    (std::cout) << (std::setw(10))
                << (std::chrono::high_resolution_clock::now()
                        .time_since_epoch()
                        .count())
                << (" ") << (std::this_thread::get_id()) << (" ") << (__FILE__)
                << (":") << (__LINE__) << (" ") << (__func__) << (" ")
                << ("out of bounds") << (" ") << (std::setw(8)) << (" idx='")
                << (idx) << ("'") << (std::setw(8)) << (" n='") << (n) << ("'")
                << (std::endl) << (std::flush);
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

    (std::cout) << (std::setw(10))
                << (std::chrono::high_resolution_clock::now()
                        .time_since_epoch()
                        .count())
                << (" ") << (std::this_thread::get_id()) << (" ") << (__FILE__)
                << (":") << (__LINE__) << (" ") << (__func__) << (" ")
                << ("fail") << (" ") << (std::setw(8)) << (" res='") << (res)
                << ("'") << (std::endl) << (std::flush);
    return nullptr;
  };
  if (!(idx < n)) {

    (std::cout) << (std::setw(10))
                << (std::chrono::high_resolution_clock::now()
                        .time_since_epoch()
                        .count())
                << (" ") << (std::this_thread::get_id()) << (" ") << (__FILE__)
                << (":") << (__LINE__) << (" ") << (__func__) << (" ")
                << ("out of bounds") << (" ") << (std::setw(8)) << (" idx='")
                << (idx) << ("'") << (std::setw(8)) << (" n='") << (n) << ("'")
                << (std::endl) << (std::flush);
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

    (std::cout) << (std::setw(10))
                << (std::chrono::high_resolution_clock::now()
                        .time_since_epoch()
                        .count())
                << (" ") << (std::this_thread::get_id()) << (" ") << (__FILE__)
                << (":") << (__LINE__) << (" ") << (__func__) << (" ")
                << ("fail") << (" ") << (std::setw(8)) << (" res='") << (res)
                << ("'") << (std::endl) << (std::flush);
    return nullptr;
  };
  if (!(idx < n)) {

    (std::cout) << (std::setw(10))
                << (std::chrono::high_resolution_clock::now()
                        .time_since_epoch()
                        .count())
                << (" ") << (std::this_thread::get_id()) << (" ") << (__FILE__)
                << (":") << (__LINE__) << (" ") << (__func__) << (" ")
                << ("out of bounds") << (" ") << (std::setw(8)) << (" idx='")
                << (idx) << ("'") << (std::setw(8)) << (" n='") << (n) << ("'")
                << (std::endl) << (std::flush);
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

    (std::cout) << (std::setw(10))
                << (std::chrono::high_resolution_clock::now()
                        .time_since_epoch()
                        .count())
                << (" ") << (std::this_thread::get_id()) << (" ") << (__FILE__)
                << (":") << (__LINE__) << (" ") << (__func__) << (" ")
                << ("fail") << (" ") << (std::setw(8)) << (" res='") << (res)
                << ("'") << (std::endl) << (std::flush);
    return -1;
  };
  if (!(idx < n)) {

    (std::cout) << (std::setw(10))
                << (std::chrono::high_resolution_clock::now()
                        .time_since_epoch()
                        .count())
                << (" ") << (std::this_thread::get_id()) << (" ") << (__FILE__)
                << (":") << (__LINE__) << (" ") << (__func__) << (" ")
                << ("out of bounds") << (" ") << (std::setw(8)) << (" idx='")
                << (idx) << ("'") << (std::setw(8)) << (" n='") << (n) << ("'")
                << (std::endl) << (std::flush);
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

    (std::cout) << (std::setw(10))
                << (std::chrono::high_resolution_clock::now()
                        .time_since_epoch()
                        .count())
                << (" ") << (std::this_thread::get_id()) << (" ") << (__FILE__)
                << (":") << (__LINE__) << (" ") << (__func__) << (" ")
                << ("fail") << (" ") << (std::setw(8)) << (" res='") << (res)
                << ("'") << (std::endl) << (std::flush);
    return -1;
  };
  if (!(idx < n)) {

    (std::cout) << (std::setw(10))
                << (std::chrono::high_resolution_clock::now()
                        .time_since_epoch()
                        .count())
                << (" ") << (std::this_thread::get_id()) << (" ") << (__FILE__)
                << (":") << (__LINE__) << (" ") << (__func__) << (" ")
                << ("out of bounds") << (" ") << (std::setw(8)) << (" idx='")
                << (idx) << ("'") << (std::setw(8)) << (" n='") << (n) << ("'")
                << (std::endl) << (std::flush);
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

    (std::cout) << (std::setw(10))
                << (std::chrono::high_resolution_clock::now()
                        .time_since_epoch()
                        .count())
                << (" ") << (std::this_thread::get_id()) << (" ") << (__FILE__)
                << (":") << (__LINE__) << (" ") << (__func__) << (" ")
                << ("fail") << (" ") << (std::setw(8)) << (" res='") << (res)
                << ("'") << (std::endl) << (std::flush);
    return NAN;
  };
  if (!(idx < n)) {

    (std::cout) << (std::setw(10))
                << (std::chrono::high_resolution_clock::now()
                        .time_since_epoch()
                        .count())
                << (" ") << (std::this_thread::get_id()) << (" ") << (__FILE__)
                << (":") << (__LINE__) << (" ") << (__func__) << (" ")
                << ("out of bounds") << (" ") << (std::setw(8)) << (" idx='")
                << (idx) << ("'") << (std::setw(8)) << (" n='") << (n) << ("'")
                << (std::endl) << (std::flush);
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

    (std::cout) << (std::setw(10))
                << (std::chrono::high_resolution_clock::now()
                        .time_since_epoch()
                        .count())
                << (" ") << (std::this_thread::get_id()) << (" ") << (__FILE__)
                << (":") << (__LINE__) << (" ") << (__func__) << (" ")
                << ("fail") << (" ") << (std::setw(8)) << (" res='") << (res)
                << ("'") << (std::endl) << (std::flush);
    return NAN;
  };
  if (!(idx < n)) {

    (std::cout) << (std::setw(10))
                << (std::chrono::high_resolution_clock::now()
                        .time_since_epoch()
                        .count())
                << (" ") << (std::this_thread::get_id()) << (" ") << (__FILE__)
                << (":") << (__LINE__) << (" ") << (__func__) << (" ")
                << ("out of bounds") << (" ") << (std::setw(8)) << (" idx='")
                << (idx) << ("'") << (std::setw(8)) << (" n='") << (n) << ("'")
                << (std::endl) << (std::flush);
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

    (std::cout) << (std::setw(10))
                << (std::chrono::high_resolution_clock::now()
                        .time_since_epoch()
                        .count())
                << (" ") << (std::this_thread::get_id()) << (" ") << (__FILE__)
                << (":") << (__LINE__) << (" ") << (__func__) << (" ")
                << ("fail") << (" ") << (std::setw(8)) << (" res='") << (res)
                << ("'") << (std::endl) << (std::flush);
    return -1;
  };
  if (!(idx < n)) {

    (std::cout) << (std::setw(10))
                << (std::chrono::high_resolution_clock::now()
                        .time_since_epoch()
                        .count())
                << (" ") << (std::this_thread::get_id()) << (" ") << (__FILE__)
                << (":") << (__LINE__) << (" ") << (__func__) << (" ")
                << ("out of bounds") << (" ") << (std::setw(8)) << (" idx='")
                << (idx) << ("'") << (std::setw(8)) << (" n='") << (n) << ("'")
                << (std::endl) << (std::flush);
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

    (std::cout) << (std::setw(10))
                << (std::chrono::high_resolution_clock::now()
                        .time_since_epoch()
                        .count())
                << (" ") << (std::this_thread::get_id()) << (" ") << (__FILE__)
                << (":") << (__LINE__) << (" ") << (__func__) << (" ")
                << ("fail") << (" ") << (std::setw(8)) << (" res='") << (res)
                << ("'") << (std::endl) << (std::flush);
    return -1;
  };
  if (!(idx < n)) {

    (std::cout) << (std::setw(10))
                << (std::chrono::high_resolution_clock::now()
                        .time_since_epoch()
                        .count())
                << (" ") << (std::this_thread::get_id()) << (" ") << (__FILE__)
                << (":") << (__LINE__) << (" ") << (__func__) << (" ")
                << ("out of bounds") << (" ") << (std::setw(8)) << (" idx='")
                << (idx) << ("'") << (std::setw(8)) << (" n='") << (n) << ("'")
                << (std::endl) << (std::flush);
    return -1;
  };
  return desc[idx].m_isParentCounter;
}
};