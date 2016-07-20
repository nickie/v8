// Copyright 2016 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#include "src/parsing/cost.h"

namespace v8 {
namespace internal {

CostCounter::map_t CostCounter::totals_;

#define DEFINE_NAME(type) #type,
static const char* node_name[] = {
  "<MODULE>",
  AST_NODE_LIST(DEFINE_NAME)
};

void CostCounter::Report(const CostCounter::map_t& counters,
                                std::ostream& os, bool verbose,
                                const char* msg) {
  // Calculate totals per cost for all node types.
  typedef std::map<cost_t, value_t> per_cost_t;
  per_cost_t all_types;
  for (auto i = counters.begin(); i != counters.end(); ++i)
    all_types[i->first.second] += i->second;
  // Report totals per cost for all node types.
  long double sum = 0;
  long double count = 0;
  os << msg;
  for (auto i = all_types.begin(); i != all_types.end(); ++i) {
    os << (int) (i->first) << ":" << i->second << ", ";
    sum += (long double)(i->first) * i->second;
    count += i->second;
  }
  os << "average: " << std::fixed << std::setprecision(3)
     << (long double)(count > 0 ? sum / count : 0) << "." << std::endl;

  if (verbose) {
    // Calculate totals per node type, then per cost.
    std::map<node_t, per_cost_t> detailed;
    for (auto i = counters.begin(); i != counters.end(); ++i)
      detailed[i->first.first][i->first.second] += i->second;
    // Report totals per node type, then per cost.
    for (auto i = detailed.begin(); i != detailed.end(); ++i) {
      long double sum = 0;
      long double count = 0;
      os << msg << "for " << node_name[i->first] << " ";
      for (auto j = i->second.begin(); j != i->second.end(); ++j) {
        os << (int) (j->first) << ":" << j->second << ", ";
        sum += (long double)(j->first) * j->second;
        count += j->second;
      }
      os << "average: " << std::fixed << std::setprecision(3)
         << (long double)(count > 0 ? sum / count : 0) << "." << std::endl;
    }
  }
}


}  // namespace internal
}  // namespace v8
