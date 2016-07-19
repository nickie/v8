// Copyright 2016 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#ifndef V8_PARSING_COST_H_
#define V8_PARSING_COST_H_

#include <iomanip>
#include <iostream>

#include "src/ast/ast.h"

namespace v8 {
namespace internal {

class CostCounter : public AstTraversalVisitor {
 public:
  explicit CostCounter(Isolate* isolate)
      : AstTraversalVisitor(isolate), counters_(CostCounter::MAX + 1) {}
  ~CostCounter() override {
    for (int i = 0; i <= CostCounter::MAX; i++) totals_[i] += counters_[i];
  }

  void Visit(AstNode* node) {
    DCHECK_NOT_NULL(node);
    int c = node->cost(false);
    if (c > CostCounter::MAX) c = CostCounter::MAX;
    std::fprintf(stderr, "counting cost %d for node type %d\n", c,
                 node->node_type());
    counters_[c]++;
    AstTraversalVisitor::Visit(node);
  }

  void Report() const { CostCounter::Report(counters_, std::cout); }
  static void Totals() { CostCounter::Report(totals_, std::cout); }
  static void Totals(std::ostream& os, const char* msg = "AST cost ") {
    CostCounter::Report(totals_, os, msg);
  }
  static void ResetTotals() { totals_.assign(CostCounter::MAX, 0); }

 private:
  template <class Alloc>
  static void Report(const std::vector<int, Alloc>& counters, std::ostream& os,
                     const char* msg = "  cost ") {
    long double sum = 0;
    long double count = 0;
    os << msg;
    for (int i = 0; i <= CostCounter::MAX; i++)
      if (counters[i] > 0) {
        os << i << ":" << counters[i] << ", ";
        sum += (long double)i * counters[i];
        count += counters[i];
      }
    os << "average: " << std::fixed << std::setprecision(3)
       << (long double)(count > 0 ? sum / count : 0) << "." << std::endl;
  }

  static const int MAX = 64;
  std::vector<int> counters_;
  static std::vector<int> totals_;
};

}  // namespace internal
}  // namespace v8

#endif  // V8_PARSING_COST_H_
