// Copyright 2016 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#include "src/ast/ast-value-factory.h"
#include "src/parsing/parser.h"
#include "src/parsing/scanner-character-streams.h"
#include "src/parsing/scanner.h"
#include "src/parsing/token.h"

namespace v8 {
namespace internal {


// A simple parser that just locates function boundaries.

#define ALLOW_FLAGS(V)                          \
  V(harmony_async_await)                        \
  V(harmony_function_sent)

#define IS_FLAGS(V)                             \
  V(sloppy)                                     \
  V(generator)                                  \
  V(async_function)

class ParserSimple {
 public:
  explicit ParserSimple(ParseInfo* info);
  ~ParserSimple();

  bool Parse(Utf16CharacterStream* stream);

 protected:
  bool ParseScript();
  bool ParseStmtOrDeclList(int end_token);
  bool ParseStmtOrDecl();
  bool ParseExpressionList();
  bool ParseExpression();
  bool ParseTerm();
  bool ParseIdentifierName();
  bool ParseRegExpLiteral();
  bool ParseTemplateLiteral();
  bool ParseArrayLiteral();
  bool ParseObjectLiteral();
  bool ParseFunctionOrGenerator(bool is_async);
  bool ParseV8Intrinsic();
  bool ParseDoExpression();
  bool ParseClass();
  bool ParsePropertyDefinition(bool in_class);
  bool ParsePropertyName(bool* is_get, bool* is_set);
  bool ParseBlock();
  bool ParseIfStatement();
  bool ParseDoWhileStatement();
  bool ParseWhileStatement();
  bool ParseForStatement();
  bool ParseWithStatement();
  bool ParseSwitchStatement();
  bool ParseCaseClause();
  bool ParseDebuggerStatement();
  bool ParseContinueStatement();
  bool ParseBreakStatement();
  bool ParseReturnStatement();
  bool ParseThrowStatement();
  bool ParseTryStatement();
  bool ParseConciseBody();

  // Auxiliary methods
  Token::Value peek();
  Token::Value PeekAhead();
  Token::Value Next();
  bool Error();
  bool Check(Token::Value token);
  bool Expect(Token::Value token);
  bool ExpectSemicolon();
  const AstRawString* GetSymbol();
  bool PeekContextualKeyword(const char* keyword);

  AstValueFactory* ast_value_factory();
  Scanner* scanner();

#define IS_METHOD_DECL(name)                            \
  bool is_##name() const { return is_##name##_; }       \
  void set_##name(bool b) { is_##name##_ = b; }

#define ALLOW_METHOD_DECL(name)                                         \
  bool allow_##name() const { return allow_##name##_; }                 \
  void set_allow_##name(bool allow) { allow_##name##_ = allow; }

  IS_FLAGS(IS_METHOD_DECL)
  ALLOW_FLAGS(ALLOW_METHOD_DECL)

#undef IS_METHOD_DECL
#undef ALLOW_METHOD_DECL

  bool peek_any_identifier();
  bool IsNextLetKeyword();

  class ForThisScope {
   public:
    typedef void (ParserSimple::*setter) (bool value);
    typedef bool (ParserSimple::*getter) () const;
    ForThisScope(ParserSimple* parser, getter g, setter s)
        : parser_(parser), setter_(s), old_value_((parser->*g)()) {}
    ~ForThisScope() { (parser_->*setter_)(old_value_); }
   private:
    ParserSimple* parser_;
    setter setter_;
    bool old_value_;
  };

#define FOR_THIS_SCOPE(getter, setter)                                  \
  ForThisScope local_##getter(this, &ParserSimple::getter, &ParserSimple::setter)

 private:
  Scanner scanner_;

#define IS_FIELD_DECL(name)                     \
  bool is_##name##_;

#define ALLOW_FIELD_DECL(name)                  \
  bool allow_##name##_;

  IS_FLAGS(IS_FIELD_DECL)
  ALLOW_FLAGS(ALLOW_FIELD_DECL)

#undef IS_FIELD_DECL
#undef ALLOW_FIELD_DECL

  AstValueFactory* ast_value_factory_;
  bool ast_value_factory_owned_;
};


// Use the simple parser as an entry point in the parser.

bool Parser::ParseSimple(ParseInfo* info) {
  ParserSimple parser(info);
  Handle<String> source(String::cast(info->script()->source()));

  base::ElapsedTimer timer;
  if (FLAG_trace_parse) {
    timer.Start();
  }

  source = String::Flatten(source);

  int start_position = 0;
  int end_position = source->length();

  if (info->is_lazy()) {
    DCHECK(!info->is_eval());
    if (info->shared_info()->is_function()) {
      Handle<SharedFunctionInfo> shared_info = info->shared_info();
      start_position = shared_info->start_position();
      end_position = shared_info->end_position();
    }
  }

  bool result;
  if (source->IsExternalTwoByteString()) {
    ExternalTwoByteStringUtf16CharacterStream stream(
        Handle<ExternalTwoByteString>::cast(source),
        start_position, end_position);
    result = parser.Parse(&stream);
  } else {
    GenericStringUtf16CharacterStream stream(
        source, start_position, end_position);
    result = parser.Parse(&stream);
  }
  if (FLAG_trace_parse) {
    double ms = timer.Elapsed().InMillisecondsF();
    PrintF("[parsing took %0.3f ms]\n", ms);
  }
  return result;
}


// Parsing functions of the simple parser.

bool ParserSimple::Parse(Utf16CharacterStream* stream) {
  std::fprintf(stderr, "Parsing simple: begin\n");
  scanner()->Initialize(stream);
  scanner()->set_allow_harmony_exponentiation_operator(
      FLAG_harmony_exponentiation_operator);
  bool result = ParseScript();
  std::fprintf(stderr, "Parsing simple: end\n");
  return result;
}

#define TRY(call) do {                          \
    if (!call) return false;                    \
  } while (false)

bool ParserSimple::ParseScript() {
  TRY(ParseStmtOrDeclList(Token::EOS));
  Expect(Token::EOS);
  return true;
}

bool ParserSimple::ParseStmtOrDeclList(int end_token) {
  bool directive_prologue = true;
  FOR_THIS_SCOPE(is_sloppy, set_sloppy);
  while (peek() != end_token) {
    // Process directives.
    if (directive_prologue) {
      if (peek() == Token::STRING &&
          (scanner()->HasAnyLineTerminatorAfterNext() ||
           PeekAhead() == Token::SEMICOLON || PeekAhead() == end_token)) {
        Next();
        if (GetSymbol() == ast_value_factory()->use_strict_string())
          set_sloppy(false);
        Check(Token::SEMICOLON);
        continue;
      } else {
        directive_prologue = false;
      }
    }
    // Process all the rest.
    TRY(ParseStmtOrDecl());
  }
  return true;
}

bool ParserSimple::ParseStmtOrDecl() {
  switch (peek()) {
    case Token::SEMICOLON:
      Next();
      break;
    case Token::FUNCTION:
      TRY(ParseFunctionOrGenerator(false));
      break;
    case Token::CLASS:
      TRY(ParseClass());
      break;
    case Token::CONST:
    case Token::VAR:
      Next();
      TRY(ParseExpressionList());
      TRY(ExpectSemicolon());
      break;
    case Token::LET:
      if (IsNextLetKeyword()) Next();
      TRY(ParseExpressionList());
      TRY(ExpectSemicolon());
      break;
    case Token::LBRACE:
      TRY(ParseBlock());
      break;
    case Token::IF:
      TRY(ParseIfStatement());
      break;
    case Token::DO:
      TRY(ParseDoWhileStatement());
      break;
    case Token::WHILE:
      TRY(ParseWhileStatement());
      break;
    case Token::FOR:
      TRY(ParseForStatement());
      break;
    case Token::CONTINUE:
      TRY(ParseContinueStatement());
      break;
    case Token::BREAK:
      TRY(ParseBreakStatement());
      break;
    case Token::RETURN:
      TRY(ParseReturnStatement());
      break;
    case Token::THROW:
      TRY(ParseThrowStatement());
      break;
    case Token::TRY:
      TRY(ParseTryStatement());
      break;
    case Token::WITH:
      TRY(ParseWithStatement());
      break;
    case Token::SWITCH:
      TRY(ParseSwitchStatement());
      break;
    case Token::DEBUGGER:
      TRY(ParseDebuggerStatement());
      break;
    case Token::CASE:
    case Token::DEFAULT:
      TRY(ParseCaseClause());
      break;
    case Token::ASYNC:
      if (allow_harmony_async_await() && PeekAhead() == Token::FUNCTION &&
          !scanner()->HasAnyLineTerminatorAfterNext()) {
        Next();
        TRY(ParseFunctionOrGenerator(true));
        break;
      }
    default:
      if (peek_any_identifier() && PeekAhead() == Token::COLON) {
        Next();
        Next();
      } else {
        TRY(ParseExpressionList());
        TRY(ExpectSemicolon());
      }
      break;
  }
  return true;
}

bool ParserSimple::ParseExpressionList() {
  while (true) {
    switch (peek()) {
      case Token::COMMA:
        Next();
        continue;
      case Token::EOS:
      case Token::SEMICOLON:
      case Token::RBRACE:
      case Token::RBRACK:
      case Token::RPAREN:
      case Token::COLON:
        break;
      default:
        TRY(ParseExpression());
        if (Check(Token::COMMA)) continue;
        break;
    }
    return true;
  }
}

bool ParserSimple::ParseExpression() {
  while (true) {
    TRY(ParseTerm());
    while (true) {
      Token::Value tok = peek();
      if (tok == Token::COMMA) return true;
      if (Token::IsBinaryOp(tok) || Token::IsCompareOp(tok) ||
          Token::IsAssignmentOp(tok)) {
        Next();
        break;
      }
      if (!scanner()->HasAnyLineTerminatorBeforeNext() && Token::IsCountOp(tok)) {
        Next();
        continue;
      }
      switch (tok) {
        case Token::LBRACK:
          Next();
          if (!Check(Token::RBRACK)) {
            TRY(ParseExpressionList());
            TRY(Expect(Token::RBRACK));
          }
          continue;
        case Token::LPAREN:
          Next();
          if (!Check(Token::RPAREN)) {
            TRY(ParseExpressionList());
            TRY(Expect(Token::RPAREN));
          }
          continue;
        case Token::PERIOD:
          Next();
          TRY(ParseIdentifierName());
          continue;
        case Token::TEMPLATE_SPAN:
        case Token::TEMPLATE_TAIL:
          TRY(ParseTemplateLiteral());
          continue;
        case Token::CONDITIONAL:
          Next();
          TRY(ParseExpression());
          TRY(Expect(Token::COLON));
          break;
        case Token::ARROW:
          TRY(ParseConciseBody());
          return true;
        default:
          return true;
      }
      break;
    }
  }
}

bool ParserSimple::ParseTerm() {
  // Parse all term prefixes.
  while (true) {
    Token::Value tok = peek();
    if (Token::IsUnaryOp(tok) || Token::IsCountOp(tok) ||
        tok == Token::ELLIPSIS) {
      Next();
    } else if (tok == Token::NEW) {
      if (PeekAhead() == Token::PERIOD) break;
      Next();
    } else if (is_generator() && tok == Token::YIELD) {
      if (!scanner()->HasAnyLineTerminatorAfterNext()) {
        switch (PeekAhead()) {
          case Token::MUL:
          case Token::EOS:
          case Token::SEMICOLON:
          case Token::RBRACE:
          case Token::RBRACK:
          case Token::RPAREN:
          case Token::COLON:
          case Token::COMMA:
            // Treat yield as identifier.
            break;
          default:
            Next();
            continue;
        }
      }
      break;
    } else if (is_async_function() && tok == Token::AWAIT) {
      switch (PeekAhead()) {
        case Token::RPAREN:
        case Token::RBRACK:
        case Token::RBRACE:
        case Token::ASSIGN:
        case Token::COMMA:
          break;
        default:
          Next();
          continue;
      }
      break;
    } else {
      break;
    }
  }
  // Parse all proper terms.
  switch (peek()) {
    case Token::ASYNC:
      if (allow_harmony_async_await() && PeekAhead() == Token::FUNCTION &&
          !scanner()->HasAnyLineTerminatorAfterNext()) {
        Next();
        TRY(ParseFunctionOrGenerator(true));
        break;
      }
      // Falls through, treating async as an identifier.
    case Token::THIS:
    case Token::NULL_LITERAL:
    case Token::TRUE_LITERAL:
    case Token::FALSE_LITERAL:
    case Token::SMI:
    case Token::NUMBER:
    case Token::IDENTIFIER:
    case Token::LET:
    case Token::STATIC:
    case Token::YIELD:
    case Token::AWAIT:
    case Token::ESCAPED_STRICT_RESERVED_WORD:
    case Token::FUTURE_STRICT_RESERVED_WORD:
    case Token::STRING:
    case Token::NEW:
    case Token::SUPER:
      Next();
      break;
    case Token::ASSIGN_DIV:
    case Token::DIV:
      TRY(ParseRegExpLiteral());
      break;
    case Token::LBRACK:
      TRY(ParseArrayLiteral());
      break;
    case Token::LBRACE:
      TRY(ParseObjectLiteral());
      break;
    case Token::LPAREN:
      Next();
      if (!Check(Token::RPAREN)) {
        TRY(ParseExpressionList());
        TRY(Expect(Token::RPAREN));
      }
      break;
    case Token::FUNCTION:
      if (allow_harmony_function_sent() && PeekAhead() == Token::PERIOD)
        Next();
      else
        TRY(ParseFunctionOrGenerator(false));
      break;
    case Token::CLASS:
      TRY(ParseClass());
      break;
    case Token::TEMPLATE_SPAN:
    case Token::TEMPLATE_TAIL:
      TRY(ParseTemplateLiteral());
      break;
    case Token::MOD:
      TRY(ParseV8Intrinsic());
      break;
    case Token::DO:
      TRY(ParseDoExpression());
      break;
    default:
      return Error();
      break;
  }
  return true;
}

bool ParserSimple::ParseIdentifierName() {
  Token::Value tok = Next();
  switch (tok) {
    case Token::IDENTIFIER:
    case Token::ASYNC:
    case Token::ENUM:
    case Token::AWAIT:
    case Token::LET:
    case Token::STATIC:
    case Token::YIELD:
    case Token::FUTURE_STRICT_RESERVED_WORD:
    case Token::ESCAPED_KEYWORD:
    case Token::ESCAPED_STRICT_RESERVED_WORD:
      break;
    default:
      if (!Token::IsKeyword(tok)) return Error();
      break;
  }
  return true;
}

bool ParserSimple::ParseRegExpLiteral() {
  DCHECK(peek() == Token::DIV || peek() == Token::ASSIGN_DIV);
  if (scanner()->ScanRegExpPattern(peek() == Token::ASSIGN_DIV)) {
    scanner()->ScanRegExpFlags();
    Next();
    return true;
  }
  return Error();
}

bool ParserSimple::ParseTemplateLiteral() {
  DCHECK(peek() == Token::TEMPLATE_SPAN || peek() == Token::TEMPLATE_TAIL);
  if (Check(Token::TEMPLATE_TAIL)) return true;
  Next();
  Token::Value next;
  do {
    next = peek();
    if (next == Token::EOS || next == Token::ILLEGAL) return Error();
    TRY(ParseExpressionList());
    if (peek() != Token::RBRACE) return Error();
    next = scanner()->ScanTemplateContinuation();
    Next();
    if (next == Token::EOS || next == Token::ILLEGAL) return Error();
  } while (next == Token::TEMPLATE_SPAN);
  DCHECK_EQ(next, Token::TEMPLATE_TAIL);
  return true;
}

bool ParserSimple::ParseArrayLiteral() {
  DCHECK_EQ(peek(), Token::LBRACK);
  Next();
  if (!Check(Token::RBRACK)) {
    TRY(ParseExpressionList());
    TRY(Expect(Token::RBRACK));
  }
  return true;
}

bool ParserSimple::ParseObjectLiteral() {
  DCHECK_EQ(peek(), Token::LBRACE);
  Next();
  while (!Check(Token::RBRACE)) {
    if (Check(Token::COMMA)) continue;
    TRY(ParsePropertyDefinition(false));
  }
  return true;
}

bool ParserSimple::ParseFunctionOrGenerator(bool is_async) {
  FOR_THIS_SCOPE(is_generator, set_generator);
  FOR_THIS_SCOPE(is_async_function, set_async_function);
  DCHECK_EQ(peek(), Token::FUNCTION);
  Next();
  bool is_generator = Check(Token::MUL);
  if (peek_any_identifier())
    TRY(ParseIdentifierName());
  if (Check(Token::LPAREN)) {
    if (!Check(Token::RPAREN)) {
      TRY(ParseExpressionList());
      TRY(Expect(Token::RPAREN));
    }
    if (Check(Token::LBRACE)) {
      set_generator(is_generator);
      set_async_function(is_async);
      int body_start = scanner()->location().beg_pos;
      if (!Check(Token::RBRACE)) {
        TRY(ParseStmtOrDeclList(Token::RBRACE));
        TRY(Expect(Token::RBRACE));
      }
      std::fprintf(stderr, "simple, function boundaries: %d, %d\n",
                   body_start, scanner()->location().end_pos);
      return true;
    }
  }
  return Error();
}

bool ParserSimple::ParseV8Intrinsic() {
  DCHECK_EQ(peek(), Token::MOD);
  Next();
  if (peek_any_identifier()) {
    TRY(ParseIdentifierName());
    if (Check(Token::LPAREN)) {
      if (!Check(Token::RPAREN)) {
        TRY(ParseExpressionList());
        TRY(Expect(Token::RPAREN));
      }
      return true;
    }
  }
  return Error();
}

bool ParserSimple::ParseDoExpression() {
  DCHECK_EQ(peek(), Token::DO);
  Next();
  TRY(ParseBlock());
  return true;
}

bool ParserSimple::ParseClass() {
  DCHECK_EQ(peek(), Token::CLASS);
  Next();
  if (peek_any_identifier()) TRY(ParseIdentifierName());
  if (Check(Token::EXTENDS)) TRY(ParseExpression());
  if (Check(Token::LBRACE)) {
    while (!Check(Token::RBRACE)) {
      if (Check(Token::SEMICOLON)) continue;
      TRY(ParsePropertyDefinition(true));
    }
    return true;
  }
  return Error();
}

bool ParserSimple::ParsePropertyDefinition(bool in_class) {
  FOR_THIS_SCOPE(is_generator, set_generator);
  FOR_THIS_SCOPE(is_async_function, set_async_function);
  bool is_static = in_class && Check(Token::STATIC);
  bool is_generator = Check(Token::MUL);
  bool is_get = false;
  bool is_set = false;
  bool is_async =
      allow_harmony_async_await() && peek() == Token::ASYNC &&
      !scanner()->HasAnyLineTerminatorAfterNext() &&
      PeekAhead() != Token::LPAREN;
  TRY(ParsePropertyName(&is_get, &is_set));
  switch (peek()) {
    case Token::COLON:
    case Token::ASSIGN:
      Next();
      TRY(ParseExpression());
      return true;
    case Token::COMMA:
    case Token::RBRACE:
    case Token::EOS:
      return true;
    default:
      break;
  }
  if (((is_get || is_set) && peek() != Token::LPAREN) ||
      (is_async && !is_generator && !is_static)) {
    bool dont_care;
    TRY(ParsePropertyName(&dont_care, &dont_care));
  }
  if (Check(Token::LPAREN)) {
    if (!Check(Token::RPAREN)) {
      TRY(ParseExpressionList());
      TRY(Expect(Token::RPAREN));
    }
    if (Check(Token::LBRACE)) {
      set_generator(is_generator);
      set_async_function(is_async);
      int body_start = scanner()->location().beg_pos;
      if (!Check(Token::RBRACE)) {
        TRY(ParseStmtOrDeclList(Token::RBRACE));
        TRY(Expect(Token::RBRACE));
      }
      std::fprintf(stderr, "simple, method boundaries: %d, %d\n",
                   body_start, scanner()->location().end_pos);
      return true;
    }
  }
  return Error();
}

bool ParserSimple::ParsePropertyName(bool* is_get, bool* is_set) {
  switch (peek()) {
    case Token::STRING:
    case Token::SMI:
    case Token::NUMBER:
      Next();
      break;
    case Token::LBRACK:
      Next();
      TRY(ParseExpressionList());
      TRY(Expect(Token::RBRACK));
      break;
    default:
      TRY(ParseIdentifierName());
      scanner()->IsGetOrSet(is_get, is_set);
      break;
  }
  return true;
}

bool ParserSimple::ParseBlock() {
  if (Check(Token::LBRACE)) {
    if (!Check(Token::RBRACE)) {
      TRY(ParseStmtOrDeclList(Token::RBRACE));
      TRY(Expect(Token::RBRACE));
    }
    return true;
  }
  return Error();
}

bool ParserSimple::ParseIfStatement() {
  DCHECK_EQ(peek(), Token::IF);
  Next();
  if (Check(Token::LPAREN)) {
    TRY(ParseExpressionList());
    if (Check(Token::RPAREN)) {
      TRY(ParseStmtOrDecl());
      if (Check(Token::ELSE)) TRY(ParseStmtOrDecl());
      return true;
    }
  }
  return Error();
}

bool ParserSimple::ParseDoWhileStatement() {
  DCHECK_EQ(peek(), Token::DO);
  Next();
  TRY(ParseStmtOrDecl());
  if (Check(Token::WHILE) && Check(Token::LPAREN)) {
    TRY(ParseExpressionList());
    TRY(Expect(Token::RPAREN));
    Check(Token::SEMICOLON);
    return true;
  }
  return Error();
}

bool ParserSimple::ParseWhileStatement() {
  DCHECK_EQ(peek(), Token::WHILE);
  Next();
  if (Check(Token::LPAREN)) {
    TRY(ParseExpressionList());
    if (Check(Token::RPAREN)) {
      TRY(ParseStmtOrDecl());
      return true;
    }
  }
  return Error();
}

bool ParserSimple::ParseForStatement() {
  DCHECK_EQ(peek(), Token::FOR);
  Next();
  if (Check(Token::LPAREN)) {
    while (peek() != Token::RPAREN) {
      switch (peek()) {
        case Token::SEMICOLON:
          Next();
          continue;
        case Token::CONST:
        case Token::VAR:
          Next();
          TRY(ParseExpressionList());
          break;
        case Token::LET:
          if (IsNextLetKeyword()) Next();
          TRY(ParseExpressionList());
          break;
        default:
          TRY(ParseExpressionList());
      }
      if (PeekContextualKeyword("of")) {
        Next();
        TRY(ParseExpressionList());
      }
    }
    if (Check(Token::RPAREN)) {
      TRY(ParseStmtOrDecl());
      return true;
    }
  }
  return Error();
}

bool ParserSimple::ParseWithStatement() {
  DCHECK_EQ(peek(), Token::WITH);
  Next();
  if (Check(Token::LPAREN)) {
    TRY(ParseExpressionList());
    if (Check(Token::RPAREN)) {
      TRY(ParseStmtOrDecl());
      return true;
    }
  }
  return Error();
}

bool ParserSimple::ParseSwitchStatement() {
  DCHECK_EQ(peek(), Token::SWITCH);
  Next();
  if (Check(Token::LPAREN)) {
    TRY(ParseExpressionList());
    if (Check(Token::RPAREN)) {
      TRY(ParseBlock());
      return true;
    }
  }
  return Error();
}

bool ParserSimple::ParseCaseClause() {
  DCHECK(peek() == Token::CASE || peek() == Token::DEFAULT);
  if (Check(Token::CASE))
    TRY(ParseExpressionList());
  else
    Next();
  TRY(Expect(Token::COLON));
  return true;
}

bool ParserSimple::ParseDebuggerStatement() {
  DCHECK_EQ(peek(), Token::DEBUGGER);
  Next();
  TRY(ExpectSemicolon());
  return true;
}

bool ParserSimple::ParseContinueStatement() {
  DCHECK_EQ(peek(), Token::CONTINUE);
  Next();
  Token::Value tok = peek();
  if (!scanner()->HasAnyLineTerminatorBeforeNext() &&
      tok != Token::SEMICOLON && tok != Token::RBRACE && tok != Token::EOS) {
    TRY(ParseIdentifierName());
  }
  TRY(ExpectSemicolon());
  return true;
}

bool ParserSimple::ParseBreakStatement() {
  DCHECK_EQ(peek(), Token::BREAK);
  Next();
  Token::Value tok = peek();
  if (!scanner()->HasAnyLineTerminatorBeforeNext() &&
      tok != Token::SEMICOLON && tok != Token::RBRACE && tok != Token::EOS)
    TRY(ParseIdentifierName());
  TRY(ExpectSemicolon());
  return true;
}

bool ParserSimple::ParseReturnStatement() {
  DCHECK_EQ(peek(), Token::RETURN);
  Next();
  Token::Value tok = peek();
  if (!scanner()->HasAnyLineTerminatorBeforeNext() &&
      tok != Token::SEMICOLON && tok != Token::RBRACE && tok != Token::EOS)
    TRY(ParseExpressionList());
  TRY(ExpectSemicolon());
  return true;
}

bool ParserSimple::ParseThrowStatement() {
  DCHECK_EQ(peek(), Token::THROW);
  Next();
  TRY(ParseExpressionList());
  TRY(ExpectSemicolon());
  return true;
}

bool ParserSimple::ParseTryStatement() {
  DCHECK_EQ(peek(), Token::TRY);
  Next();
  TRY(ParseBlock());
  if (Check(Token::CATCH) && Check(Token::LPAREN)) {
    TRY(ParseExpressionList());
    TRY(Expect(Token::RPAREN));
    TRY(ParseBlock());
  }
  if (Check(Token::FINALLY)) TRY(ParseBlock());
  return true;
}

bool ParserSimple::ParseConciseBody() {
  DCHECK_EQ(peek(), Token::ARROW);
  Next();
  int body_start = scanner()->location().beg_pos;
  if (peek() == Token::LBRACE)
    TRY(ParseBlock());
  else
    TRY(ParseExpression());
  std::fprintf(stderr, "simple, arrow function boundaries: %d, %d\n",
               body_start, scanner()->location().end_pos);
  return true;
}


// Implementation of auxiliary methods.

#define IS_FIELD_INIT(name)                     \
  is_##name##_(false),
#define ALLOW_FIELD_INIT(name)                  \
  allow_##name##_(false),

ParserSimple::ParserSimple(ParseInfo* info)
    : scanner_(info->unicode_cache()),
      IS_FLAGS(IS_FIELD_INIT)
      ALLOW_FLAGS(ALLOW_FIELD_INIT)
      ast_value_factory_(info->ast_value_factory()),
      ast_value_factory_owned_(false) {
  if (ast_value_factory_ == NULL) {
    ast_value_factory_ = new AstValueFactory(info->zone(), info->hash_seed());
    ast_value_factory_owned_ = true;
  }
  set_sloppy(true);
}

#undef IS_FIELD_INIT
#undef ALLOW_FIELD_INIT

ParserSimple::~ParserSimple() {
  if (ast_value_factory_owned_) delete ast_value_factory_;
}

Token::Value ParserSimple::peek() {
  return scanner()->peek();
}

Token::Value ParserSimple::PeekAhead() {
  return scanner()->PeekAhead();
}

Token::Value ParserSimple::Next() {
  return scanner()->Next();
}

bool ParserSimple::Check(Token::Value token) {
  if (peek() != token) return false;
  Next();
  return true;
}

bool ParserSimple::Error() {
  std::fprintf(stderr, "Parsing simple: error at %d, unexpected token %s\n",
               scanner()->peek_location().beg_pos, Token::Name(peek()));
  return false;
}

bool ParserSimple::Expect(Token::Value token) {
  if (Check(token)) return true;
  std::fprintf(stderr, "Parsing simple: error at %d, expected %s, found %s\n",
               scanner()->peek_location().beg_pos, Token::Name(token),
               Token::Name(peek()));
  return false;
}

bool ParserSimple::ExpectSemicolon() {
  while (true) {
    Token::Value tok = peek();
    switch (tok) {
      case Token::SEMICOLON:
        Next();
        // Fall through.
      case Token::RBRACE:
      case Token::EOS:
        return true;
      default:
        if (scanner()->HasAnyLineTerminatorBeforeNext())
          return true;
        break;
    }
    return Error();
  }
}

const AstRawString* ParserSimple::GetSymbol() {
  const AstRawString* result = scanner()->CurrentSymbol(ast_value_factory_);
  DCHECK(result != NULL);
  return result;
}

Scanner* ParserSimple::scanner() {
  return &scanner_;
}

AstValueFactory* ParserSimple::ast_value_factory() {
  return ast_value_factory_;
}

bool ParserSimple::IsNextLetKeyword() {
  DCHECK_EQ(peek(), Token::LET);
  switch (PeekAhead()) {
    case Token::LBRACE:
    case Token::LBRACK:
    case Token::IDENTIFIER:
    case Token::STATIC:
    case Token::LET:  // `let let;` is disallowed by static semantics, but the
                      // token must be first interpreted as a keyword in order
                      // for those semantics to apply. This ensures that ASI is
                      // not honored when a LineTerminator separates the
                      // tokens.
    case Token::YIELD:
    case Token::AWAIT:
    case Token::ASYNC:
      return true;
    case Token::FUTURE_STRICT_RESERVED_WORD:
      return is_sloppy();
    default:
      return false;
  }
}

bool ParserSimple::peek_any_identifier() {
  switch (peek()) {
    case Token::IDENTIFIER:
    case Token::ENUM:
    case Token::AWAIT:
    case Token::ASYNC:
    case Token::FUTURE_STRICT_RESERVED_WORD:
    case Token::LET:
    case Token::STATIC:
    case Token::YIELD:
      return true;
    default:
      return false;
  }
}

bool ParserSimple::PeekContextualKeyword(const char* keyword) {
  return peek() == Token::IDENTIFIER &&
      scanner()->is_next_contextual_keyword(CStrVector(keyword));
}


}  // namespace internal
}  // namespace v8
