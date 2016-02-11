// Copyright 2015 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#undef NICKIE_DEBUG

#ifdef NICKIE_DEBUG
#define DEBUG_THIS __FILE__, __LINE__, this
#else
#define DEBUG_THIS this
#endif

#ifndef V8_PARSING_EXPRESSION_CLASSIFIER_H
#define V8_PARSING_EXPRESSION_CLASSIFIER_H

#include "src/messages.h"
#include "src/parsing/scanner.h"
#include "src/parsing/token.h"

namespace v8 {
namespace internal {

#define ERROR_CODES(T)                       \
  T(ExpressionProduction, 0)                 \
  T(FormalParameterInitializerProduction, 1) \
  T(BindingPatternProduction, 2)             \
  T(AssignmentPatternProduction, 3)          \
  T(DistinctFormalParametersProduction, 4)   \
  T(StrictModeFormalParametersProduction, 5) \
  T(StrongModeFormalParametersProduction, 6) \
  T(ArrowFormalParametersProduction, 7)      \
  T(LetPatternProduction, 8)                 \
  T(CoverInitializedNameProduction, 9)

template <typename Traits>
class ExpressionClassifier {
 public:
  enum ErrorKind : unsigned {
#define DEFINE_ERROR_KIND(NAME, CODE) k##NAME = CODE,
    ERROR_CODES(DEFINE_ERROR_KIND)
#undef DEFINE_ERROR_KIND
    kUnusedError = 15  // Larger than error codes; should fit in 4 bits
  };

  struct Error {
    V8_INLINE Error()
        : location(Scanner::Location::invalid()),
          message(MessageTemplate::kNone),
          kind(kUnusedError),
          type(kSyntaxError),
          arg(nullptr) {}
    V8_INLINE explicit Error(Scanner::Location loc,
                             MessageTemplate::Template msg, ErrorKind k,
                             const char* a = nullptr,
                             ParseErrorType t = kSyntaxError)
        : location(loc), message(msg), kind(k), type(t), arg(a) {}

    Scanner::Location location;
    MessageTemplate::Template message : 26;
    ErrorKind kind : 4;
    ParseErrorType type : 2;
    const char* arg;
  };

  typedef int size_type;

  enum TargetProduction : unsigned {
#define DEFINE_PRODUCTION(NAME, CODE) NAME = 1 << CODE,
    ERROR_CODES(DEFINE_PRODUCTION)
#undef DEFINE_PRODUCTION

    ExpressionProductions =
        (ExpressionProduction | FormalParameterInitializerProduction),
    PatternProductions = (BindingPatternProduction |
                          AssignmentPatternProduction | LetPatternProduction),
    FormalParametersProductions = (DistinctFormalParametersProduction |
                                   StrictModeFormalParametersProduction |
                                   StrongModeFormalParametersProduction),
    StandardProductions = ExpressionProductions | PatternProductions,
    AllProductions =
        (StandardProductions | FormalParametersProductions |
         ArrowFormalParametersProduction | CoverInitializedNameProduction)
  };

  enum FunctionProperties : unsigned {
    NonSimpleParameter = 1 << 0
  };

  explicit ExpressionClassifier(
#ifdef NICKIE_DEBUG
                                        const char *filename, int line,
#endif
                                        const Traits* t)
      : reported_errors_(t->GetReportedErrorList()),
        zone_(t->zone()),
        invalid_productions_(0),
        function_properties_(0),
        duplicate_finder_(nullptr) {
    mine_begin_ = mine_end_ = reported_errors_->length();
#ifdef NICKIE_DEBUG
    fprintf(stderr, "create classifier %p %u- list %p at %s:%d\n", this,
            mine_begin_, reported_errors_, filename, line);
#endif
  }

  ExpressionClassifier(
#ifdef NICKIE_DEBUG
                               const char *filename, int line,
#endif
                               const Traits* t,
                               DuplicateFinder* duplicate_finder)
      : reported_errors_(t->GetReportedErrorList()),
        zone_(t->zone()),
        invalid_productions_(0),
        function_properties_(0),
        duplicate_finder_(duplicate_finder) {
    mine_begin_ = mine_end_ = reported_errors_->length();
#ifdef NICKIE_DEBUG
    fprintf(stderr, "create classifier %p %u- list %p at %s:%d\n", this,
            mine_begin_, reported_errors_, filename, line);
#endif
  }

  ~ExpressionClassifier() { Discard(); }

  V8_INLINE bool is_valid(unsigned productions) const {
    return (invalid_productions_ & productions) == 0;
  }

  V8_INLINE DuplicateFinder* duplicate_finder() const {
    return duplicate_finder_;
  }

  V8_INLINE bool is_valid_expression() const {
    return is_valid(ExpressionProduction);
  }

  V8_INLINE bool is_valid_formal_parameter_initializer() const {
    return is_valid(FormalParameterInitializerProduction);
  }

  V8_INLINE bool is_valid_binding_pattern() const {
    return is_valid(BindingPatternProduction);
  }

  V8_INLINE bool is_valid_assignment_pattern() const {
    return is_valid(AssignmentPatternProduction);
  }

  V8_INLINE bool is_valid_arrow_formal_parameters() const {
    return is_valid(ArrowFormalParametersProduction);
  }

  V8_INLINE bool is_valid_formal_parameter_list_without_duplicates() const {
    return is_valid(DistinctFormalParametersProduction);
  }

  // Note: callers should also check
  // is_valid_formal_parameter_list_without_duplicates().
  V8_INLINE bool is_valid_strict_mode_formal_parameters() const {
    return is_valid(StrictModeFormalParametersProduction);
  }

  // Note: callers should also check is_valid_strict_mode_formal_parameters()
  // and is_valid_formal_parameter_list_without_duplicates().
  V8_INLINE bool is_valid_strong_mode_formal_parameters() const {
    return is_valid(StrongModeFormalParametersProduction);
  }

  V8_INLINE bool is_valid_let_pattern() const {
    return is_valid(LetPatternProduction);
  }

  V8_INLINE const Error& expression_error() const {
    return reported_error(kExpressionProduction);
  }

  V8_INLINE const Error& formal_parameter_initializer_error() const {
    return reported_error(kFormalParameterInitializerProduction);
  }

  V8_INLINE const Error& binding_pattern_error() const {
    return reported_error(kBindingPatternProduction);
  }

  V8_INLINE const Error& assignment_pattern_error() const {
    return reported_error(kAssignmentPatternProduction);
  }

  V8_INLINE const Error& arrow_formal_parameters_error() const {
    return reported_error(kArrowFormalParametersProduction);
  }

  V8_INLINE const Error& duplicate_formal_parameter_error() const {
    return reported_error(kDistinctFormalParametersProduction);
  }

  V8_INLINE const Error& strict_mode_formal_parameter_error() const {
    return reported_error(kStrictModeFormalParametersProduction);
  }

  V8_INLINE const Error& strong_mode_formal_parameter_error() const {
    return reported_error(kStrongModeFormalParametersProduction);
  }

  V8_INLINE const Error& let_pattern_error() const {
    return reported_error(kLetPatternProduction);
  }

  V8_INLINE bool has_cover_initialized_name() const {
    return !is_valid(CoverInitializedNameProduction);
  }

  V8_INLINE const Error& cover_initialized_name_error() const {
    return reported_error(kCoverInitializedNameProduction);
  }

  V8_INLINE bool is_simple_parameter_list() const {
    return !(function_properties_ & NonSimpleParameter);
  }

  V8_INLINE void RecordNonSimpleParameter() {
    function_properties_ |= NonSimpleParameter;
  }

  void RecordExpressionError(const Scanner::Location& loc,
                             MessageTemplate::Template message,
                             const char* arg = nullptr) {
    if (!is_valid_expression()) return;
    invalid_productions_ |= ExpressionProduction;
    Add(Error(loc, message, kExpressionProduction, arg));
  }

  void RecordExpressionError(const Scanner::Location& loc,
                             MessageTemplate::Template message,
                             ParseErrorType type, const char* arg = nullptr) {
    if (!is_valid_expression()) return;
    invalid_productions_ |= ExpressionProduction;
    Add(Error(loc, message, kExpressionProduction, arg, type));
  }

  void RecordFormalParameterInitializerError(const Scanner::Location& loc,
                                             MessageTemplate::Template message,
                                             const char* arg = nullptr) {
    if (!is_valid_formal_parameter_initializer()) return;
    invalid_productions_ |= FormalParameterInitializerProduction;
    Add(Error(loc, message, kFormalParameterInitializerProduction, arg));
  }

  static Error BindingPatternError(const Scanner::Location& loc,
                                   MessageTemplate::Template message,
                                   const char* arg = nullptr) {
    return Error(loc, message, kBindingPatternProduction, arg);
  }

  void RecordBindingPatternError(const Error& e) {
    if (!is_valid_binding_pattern()) return;
    invalid_productions_ |= BindingPatternProduction;
    Add(e);
  }

  void RecordBindingPatternError(const Scanner::Location& loc,
                                 MessageTemplate::Template message,
                                 const char* arg = nullptr) {
    RecordBindingPatternError(BindingPatternError(loc, message, arg));
  }

  void RecordAssignmentPatternError(const Scanner::Location& loc,
                                    MessageTemplate::Template message,
                                    const char* arg = nullptr) {
    if (!is_valid_assignment_pattern()) return;
    invalid_productions_ |= AssignmentPatternProduction;
    Add(Error(loc, message, kAssignmentPatternProduction, arg));
  }

  void RecordPatternError(const Scanner::Location& loc,
                          MessageTemplate::Template message,
                          const char* arg = nullptr) {
    RecordBindingPatternError(loc, message, arg);
    RecordAssignmentPatternError(loc, message, arg);
  }

  void RecordArrowFormalParametersError(const Scanner::Location& loc,
                                        MessageTemplate::Template message,
                                        const char* arg = nullptr) {
    if (!is_valid_arrow_formal_parameters()) return;
    invalid_productions_ |= ArrowFormalParametersProduction;
    Add(Error(loc, message, kArrowFormalParametersProduction, arg));
  }

  void RecordDuplicateFormalParameterError(const Scanner::Location& loc) {
    if (!is_valid_formal_parameter_list_without_duplicates()) return;
    invalid_productions_ |= DistinctFormalParametersProduction;
    Add(Error(loc, MessageTemplate::kParamDupe,
              kDistinctFormalParametersProduction));
  }

  // Record a binding that would be invalid in strict mode.  Confusingly this
  // is not the same as StrictFormalParameterList, which simply forbids
  // duplicate bindings.
  void RecordStrictModeFormalParameterError(const Scanner::Location& loc,
                                            MessageTemplate::Template message,
                                            const char* arg = nullptr) {
    if (!is_valid_strict_mode_formal_parameters()) return;
    invalid_productions_ |= StrictModeFormalParametersProduction;
    Add(Error(loc, message, kStrictModeFormalParametersProduction, arg));
  }

  void RecordStrongModeFormalParameterError(const Scanner::Location& loc,
                                            MessageTemplate::Template message,
                                            const char* arg = nullptr) {
    if (!is_valid_strong_mode_formal_parameters()) return;
    invalid_productions_ |= StrongModeFormalParametersProduction;
    Add(Error(loc, message, kStrongModeFormalParametersProduction, arg));
  }

  void RecordLetPatternError(const Scanner::Location& loc,
                             MessageTemplate::Template message,
                             const char* arg = nullptr) {
    if (!is_valid_let_pattern()) return;
    invalid_productions_ |= LetPatternProduction;
    Add(Error(loc, message, kLetPatternProduction, arg));
  }

  void RecordCoverInitializedNameError(const Scanner::Location& loc,
                                       MessageTemplate::Template message,
                                       const char* arg = nullptr) {
    if (has_cover_initialized_name()) return;
    invalid_productions_ |= CoverInitializedNameProduction;
    Add(Error(loc, message, kCoverInitializedNameProduction, arg));
  }

  void ForgiveCoverInitializedNameError() {
    if (!(invalid_productions_ & CoverInitializedNameProduction)) return;
    invalid_productions_ &= ~CoverInitializedNameProduction;
    Error& e = reported_error(kCoverInitializedNameProduction);
    e.kind = kUnusedError;
  }

  void ForgiveAssignmentPatternError() {
    if (!(invalid_productions_ & AssignmentPatternProduction)) return;
    invalid_productions_ &= ~AssignmentPatternProduction;
    Error& e = reported_error(kAssignmentPatternProduction);
    e.kind = kUnusedError;
  }

  void Accumulate(ExpressionClassifier& inner,
                  unsigned productions = StandardProductions) {
#ifdef NICKIE_DEBUG
    fprintf(stderr, "accumulate classifier %p %u-%u in %p %u-%u\n",
            &inner, inner.mine_begin_, inner.mine_end_,
            this, mine_begin_, mine_end_);
#endif
    DCHECK_EQ(inner.reported_errors_, reported_errors_);
    DCHECK_EQ(inner.mine_begin_, mine_end_);
    DCHECK_EQ(inner.mine_end_, reported_errors_->length());
    // Propagate errors from inner, but don't overwrite already recorded
    // errors.
    unsigned non_arrow_inner_invalid_productions =
        inner.invalid_productions_ & ~ArrowFormalParametersProduction;
    size_type next = inner.mine_begin_;
    if (non_arrow_inner_invalid_productions) {
      unsigned non_arrow_productions =
          productions & ~ArrowFormalParametersProduction;
      unsigned errors =
          non_arrow_productions & non_arrow_inner_invalid_productions;
      errors &= ~invalid_productions_;
      // As an exception to the above, the result continues to be a valid arrow
      // formal parameters if the inner expression is a valid binding pattern.
      if (productions & ArrowFormalParametersProduction &&
          is_valid_arrow_formal_parameters()) {
        // Also copy function properties if expecting an arrow function
        // parameter.
        function_properties_ |= inner.function_properties_;

        if (!inner.is_valid_binding_pattern())
          errors |= ArrowFormalParametersProduction;
      }

      if (errors != 0) {
        invalid_productions_ |= errors;
        size_type arrow_index = inner.mine_end_;
        for (size_type i = inner.mine_begin_; i < inner.mine_end_; i++) {
          if (reported_errors_->at(i).kind == kUnusedError ||
              reported_errors_->at(i).kind == kArrowFormalParametersProduction)
            continue;
          if (errors & (1 << reported_errors_->at(i).kind))
            Move(next++, i);
          if (reported_errors_->at(i).kind == kBindingPatternProduction &&
              errors & ArrowFormalParametersProduction) {
            if (next <= i) {
              Move(next, i);
              reported_errors_->at(next++).kind =
                  kArrowFormalParametersProduction;
            } else {
              DCHECK_EQ(next, i+1);
              arrow_index = i;
            }
          }
        }
        if (arrow_index < inner.mine_end_) {
          Add(reported_errors_->at(arrow_index));
          reported_errors_->at(next++).kind = kArrowFormalParametersProduction;
        }
      }
    }
    DCHECK_EQ(mine_end_, next);
    reported_errors_->Rewind(next);
    inner.mine_begin_ = inner.mine_end_ = next;
#ifdef NICKIE_DEBUG
    fprintf(stderr, "now classifier %p %u-%u\n", this, mine_begin_, mine_end_);
#endif
  }

  V8_INLINE void Discard() {
#ifdef NICKIE_DEBUG
    fprintf(stderr, "discard classifier %p %u-%u\n", this,
            mine_begin_, mine_end_);
#endif
    if (mine_end_ == reported_errors_->length()) {
      reported_errors_->Rewind(mine_begin_);
      mine_end_ = mine_begin_;
    }
    DCHECK_EQ(mine_begin_, mine_end_);
  }

 private:
  V8_INLINE Error& reported_error(ErrorKind kind) const {
    if (invalid_productions_ & (1 << kind)) {
      for (size_type i = mine_begin_; i < mine_end_; i++) {
        if (reported_errors_->at(i).kind == kind)
          return reported_errors_->at(i);
      }
    }
    // We should only be looking for an error when we know that one has
    // been reported.  But we're not...  So this is to make sure we have
    // the same behaviour.
    static Error none;
    return none;
  }

  V8_INLINE void Add(const Error& e) {
    DCHECK_EQ(mine_end_, reported_errors_->length());
    reported_errors_->Add(e, zone_);
    mine_end_++;
#ifdef NICKIE_DEBUG
    fprintf(stderr, "adding, now classifier %p %u-%u\n",
            this, mine_begin_, mine_end_);
#endif
  }

  V8_INLINE void Move(size_type next, size_type i) {
    DCHECK_EQ(mine_end_, next);
    DCHECK_LE(next, i);
    DCHECK_LT(i, reported_errors_->length());
    if (next < i) reported_errors_->at(next++) = reported_errors_->at(i);
    mine_end_++;
  }

  ZoneList<Error>* reported_errors_;
  Zone* zone_;

  unsigned invalid_productions_ : 14;
  unsigned function_properties_ : 2;

  size_type mine_begin_;
  size_type mine_end_;

  DuplicateFinder* duplicate_finder_;
};


#undef ERROR_CODES


}  // namespace internal
}  // namespace v8

#endif  // V8_PARSING_EXPRESSION_CLASSIFIER_H
