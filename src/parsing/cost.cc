// Copyright 2012 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#include "src/parsing/cost.h"

namespace v8 {
namespace internal {

std::vector<int> CostCounter::totals_(1 + CostCounter::MAX);

}  // namespace internal
}  // namespace v8
