// Copyright 2012 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#ifndef V8_PARSING_COST_H_
#define V8_PARSING_COST_H_

#include "src/ast/ast.h"

namespace v8 {
namespace internal {

class CostCounter: public AstTraversalVisitor {
 public:
  CostCounter(Isolate* isolate) : AstTraversalVisitor(isolate),
                                  counters_(CostCounter::MAX + 1) {}
  ~CostCounter() override {
    for (int i = 0; i <= CostCounter::MAX; i++)
      totals_[i] += counters_[i];
  }

  void Visit(AstNode* node) override {
    int c = node->cost(false);
    if (c > CostCounter::MAX) c = CostCounter::MAX;
    counters_[c]++;
    if (!CheckStackOverflow()) node->Accept(this);
  }

  void Report() const { CostCounter::Report(counters_); }
  static void Totals() { CostCounter::Report(totals_); }

 private:
  template<class Alloc>
  static void Report(const std::vector<int, Alloc>& counters) {
    long double sum = 0;
    long double count = 0;
    PrintF("  cost ");
    for (int i = 0; i <= CostCounter::MAX; i++)
      if (counters[i] > 0) {
        PrintF("%d:%d, ", i, counters[i]);
        sum += (long double) i * counters[i];
        count += counters[i];
      }
    PrintF("average: %0.3Lf.\n", (long double) (count > 0 ? sum / count : 0));
  }

  static const int MAX = 64;
  std::vector<int> counters_;
  static std::vector<int> totals_;
};

}  // namespace internal
}  // namespace v8

#endif  // V8_PARSING_COST_H_
