// Copyright 2015 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#ifndef V8_PARSING_EXPRESSION_CLASSIFIER_H
#define V8_PARSING_EXPRESSION_CLASSIFIER_H

#include "src/messages.h"
#include "src/parsing/scanner.h"
#include "src/parsing/token.h"

namespace v8 {
namespace internal {


#define ERROR_CODES(T)                          \
  T(ExpressionProduction, 0)                    \
  T(FormalParameterInitializerProduction, 1)    \
  T(BindingPatternProduction, 2)                \
  T(AssignmentPatternProduction, 3)             \
  T(DistinctFormalParametersProduction, 4)      \
  T(StrictModeFormalParametersProduction, 5)    \
  T(StrongModeFormalParametersProduction, 6)    \
  T(ArrowFormalParametersProduction, 7)         \
  T(LetPatternProduction, 8)                    \
  T(CoverInitializedNameProduction, 9)


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
                             MessageTemplate::Template msg,
                             ErrorKind k, const char* a = nullptr,
                             ParseErrorType t = kSyntaxError)
        : location(loc), message(msg), kind(k), type(t), arg(a) {}

    Scanner::Location location;
    MessageTemplate::Template message : 26;
    ErrorKind kind : 4;
    ParseErrorType type : 2;
    const char* arg;
  };

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

  ExpressionClassifier()
      : invalid_productions_(0),
        function_properties_(0),
        reported_errors_(3),  // statistics: 3 covers 90-99% of cases
        duplicate_finder_(nullptr) {}

  explicit ExpressionClassifier(DuplicateFinder* duplicate_finder)
      : invalid_productions_(0),
        function_properties_(0),
        reported_errors_(3),  // statistics: 3 covers 90-99% of cases
        duplicate_finder_(duplicate_finder) {}

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

  V8_INLINE void RecordExpressionError(const Scanner::Location& loc,
                                       MessageTemplate::Template message,
                                       const char* arg = nullptr) {
    if (!is_valid_expression()) return;
    invalid_productions_ |= ExpressionProduction;
    reported_errors_.Add(Error(loc, message, kExpressionProduction, arg));
  }

  void RecordExpressionError(const Scanner::Location& loc,
                             MessageTemplate::Template message,
                             ParseErrorType type, const char* arg = nullptr) {
    if (!is_valid_expression()) return;
    invalid_productions_ |= ExpressionProduction;
    reported_errors_.Add(
        Error(loc, message, kExpressionProduction, arg, type));
  }

  void RecordFormalParameterInitializerError(const Scanner::Location& loc,
                                             MessageTemplate::Template message,
                                             const char* arg = nullptr) {
    if (!is_valid_formal_parameter_initializer()) return;
    invalid_productions_ |= FormalParameterInitializerProduction;
    reported_errors_.Add(
        Error(loc, message, kFormalParameterInitializerProduction, arg));
  }

  void RecordBindingPatternError(const Scanner::Location& loc,
                                 MessageTemplate::Template message,
                                 const char* arg = nullptr) {
    if (!is_valid_binding_pattern()) return;
    invalid_productions_ |= BindingPatternProduction;
    reported_errors_.Add(Error(loc, message, kBindingPatternProduction, arg));
  }

  void RecordAssignmentPatternError(const Scanner::Location& loc,
                                    MessageTemplate::Template message,
                                    const char* arg = nullptr) {
    if (!is_valid_assignment_pattern()) return;
    invalid_productions_ |= AssignmentPatternProduction;
    reported_errors_.Add(
        Error(loc, message, kAssignmentPatternProduction, arg));
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
    reported_errors_.Add(
        Error(loc, message, kArrowFormalParametersProduction, arg));
  }

  void RecordDuplicateFormalParameterError(const Scanner::Location& loc) {
    if (!is_valid_formal_parameter_list_without_duplicates()) return;
    invalid_productions_ |= DistinctFormalParametersProduction;
    reported_errors_.Add(Error(loc, MessageTemplate::kParamDupe,
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
    reported_errors_.Add(
        Error(loc, message, kStrictModeFormalParametersProduction, arg));
  }

  void RecordStrongModeFormalParameterError(const Scanner::Location& loc,
                                            MessageTemplate::Template message,
                                            const char* arg = nullptr) {
    if (!is_valid_strong_mode_formal_parameters()) return;
    invalid_productions_ |= StrongModeFormalParametersProduction;
    reported_errors_.Add(
        Error(loc, message, kStrongModeFormalParametersProduction, arg));
  }

  void RecordLetPatternError(const Scanner::Location& loc,
                             MessageTemplate::Template message,
                             const char* arg = nullptr) {
    if (!is_valid_let_pattern()) return;
    invalid_productions_ |= LetPatternProduction;
    reported_errors_.Add(Error(loc, message, kLetPatternProduction, arg));
  }

  void RecordCoverInitializedNameError(const Scanner::Location& loc,
                                       MessageTemplate::Template message,
                                       const char* arg = nullptr) {
    if (has_cover_initialized_name()) return;
    invalid_productions_ |= CoverInitializedNameProduction;
    reported_errors_.Add(
        Error(loc, message, kCoverInitializedNameProduction, arg));
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

  void Accumulate(const ExpressionClassifier& inner,
                  unsigned productions = StandardProductions) {
    // Propagate errors from inner, but don't overwrite already recorded
    // errors.
    unsigned non_arrow_inner_invalid_productions =
        inner.invalid_productions_ & ~ArrowFormalParametersProduction;
    if (non_arrow_inner_invalid_productions == 0) return;
    unsigned non_arrow_productions =
        productions & ~ArrowFormalParametersProduction;
    unsigned errors =
        non_arrow_productions & non_arrow_inner_invalid_productions;
    errors &= ~invalid_productions_;
    if (errors != 0) {
      invalid_productions_ |= errors;
      for (List<Error>::iterator i = inner.reported_errors_.begin();
           i != inner.reported_errors_.end(); i++) {
        if (i->kind == kUnusedError) continue;
        if (errors & (1 << i->kind)) reported_errors_.Add(*i);
      }
    }

    // As an exception to the above, the result continues to be a valid arrow
    // formal parameters if the inner expression is a valid binding pattern.
    if (productions & ArrowFormalParametersProduction &&
        is_valid_arrow_formal_parameters()) {
      // Also copy function properties if expecting an arrow function
      // parameter.
      function_properties_ |= inner.function_properties_;

      if (!inner.is_valid_binding_pattern()) {
        invalid_productions_ |= ArrowFormalParametersProduction;
        const Error& e = inner.reported_error(kBindingPatternProduction);
        if (e.kind != kUnusedError) reported_errors_.Add(e);
      }
    }
  }

 private:
  V8_INLINE Error& reported_error(ErrorKind kind) const {
    for (List<Error>::iterator i = reported_errors_.begin();
         i != reported_errors_.end(); i++) {
      if (i->kind == kind)
        return *i;
    }
    // We should only be looking for an error when we know that one has
    // been reported.  But we're not...  So this is to make sure we have
    // the same behaviour.
    static Error none;
    return none;
  }

  unsigned invalid_productions_ : 14;
  unsigned function_properties_ : 2;
  List<Error> reported_errors_;
  DuplicateFinder* duplicate_finder_;
};


#undef ERROR_CODES


}  // namespace internal
}  // namespace v8

#endif  // V8_PARSING_EXPRESSION_CLASSIFIER_H
