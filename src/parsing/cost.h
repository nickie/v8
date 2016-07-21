// Copyright 2016 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#ifndef V8_PARSING_COST_H_
#define V8_PARSING_COST_H_

#include <iomanip>
#include <iostream>

#include "src/ast/ast-traversal-visitor.h"

namespace v8 {
namespace internal {

class CostCounter : public AstTraversalVisitor<CostCounter> {
 public:
  explicit CostCounter(Isolate* isolate) : AstTraversalVisitor(isolate) {}
  ~CostCounter() {
    for (auto i = counters_.begin(); i != counters_.end(); ++i)
      totals_[i->first] += i->second;
  }

  bool VisitNode(AstNode* node) {
    DCHECK_NOT_NULL(node);
    int c = node->cost(false);
    if (c > CostCounter::MAX) c = CostCounter::MAX;
    counters_[std::make_pair(node->node_type(), c)]++;
    return true;
  }

  void Report() const { CostCounter::Report(counters_, std::cout); }
  static void Totals() { CostCounter::Report(totals_, std::cout, true); }
  static void Totals(std::ostream& os, const char* msg = "AST cost ") {
    CostCounter::Report(totals_, os, true, msg);
  }
  static void ResetTotals() { totals_.clear(); }

 private:
  typedef uint8_t cost_t;
  typedef uint8_t node_t;
  typedef uint32_t value_t;
  typedef std::pair<node_t, cost_t> key_t;
  typedef std::map<key_t, value_t> map_t;
  static const int MAX = std::numeric_limits<cost_t>::max();

  static void Report(const map_t& counters, std::ostream& os,
                     bool verbose = false, const char* msg = "  cost ");

  map_t counters_;
  static map_t totals_;
};

}  // namespace internal
}  // namespace v8

#endif  // V8_PARSING_COST_H_
