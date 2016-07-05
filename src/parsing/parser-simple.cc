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

  int Parse(Utf16CharacterStream* stream);

 protected:
  void ParseScript();
  void ParseStmtOrDeclList(int end_token);
  void ParseStmtOrDecl();
  void ParseExpressionList();
  void ParseExpression();
  void ParseTerm();
  void ParseIdentifierName();
  void ParseRegExpLiteral();
  void ParseTemplateLiteral();
  void ParseArrayLiteral();
  void ParseObjectLiteral();
  void ParseFunctionOrGenerator();
  void ParseV8Intrinsic();
  void ParseDoExpression();
  void ParseClass();
  void ParsePropertyDefinition(bool in_class);
  void ParsePropertyName(bool* is_get, bool* is_set);
  void ParseBlock();
  void ParseIfStatement();
  void ParseDoWhileStatement();
  void ParseWhileStatement();
  void ParseForStatement();
  void ParseWithStatement();
  void ParseSwitchStatement();
  void ParseCaseClause();
  void ParseDebuggerStatement();
  void ParseContinueStatement();
  void ParseBreakStatement();
  void ParseReturnStatement();
  void ParseThrowStatement();
  void ParseTryStatement();
  void ParseConciseBody();

  // Auxiliary methods
  Token::Value peek();
  Token::Value PeekAhead();
  Token::Value Next();
  void Error();
  bool Check(Token::Value token);
  void Expect(Token::Value token);
  void ExpectSemicolon();
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

  int up_to_position;
  if (source->IsExternalTwoByteString()) {
    ExternalTwoByteStringUtf16CharacterStream stream(
        Handle<ExternalTwoByteString>::cast(source),
        start_position, end_position);
    up_to_position = parser.Parse(&stream);
  } else {
    GenericStringUtf16CharacterStream stream(
        source, start_position, end_position);
    up_to_position = parser.Parse(&stream);
  }
  if (FLAG_trace_parse) {
    double ms = timer.Elapsed().InMillisecondsF();
    PrintF("[parsing took %0.3f ms]\n", ms);
  }
  return up_to_position == end_position;
}


// Parsing functions of the simple parser.

int ParserSimple::Parse(Utf16CharacterStream* stream) {
  std::fprintf(stderr, "Parsing simple: begin\n");
  scanner()->Initialize(stream);
  scanner()->set_allow_harmony_exponentiation_operator(
      FLAG_harmony_exponentiation_operator);
  ParseScript();
  std::fprintf(stderr, "Parsing simple: end\n");
  return scanner()->peek_location().beg_pos;
}

void ParserSimple::ParseScript() {
  ParseStmtOrDeclList(Token::EOS);
}

void ParserSimple::ParseStmtOrDeclList(int end_token) {
  bool directive_prologue = true;
  while (peek() != end_token) {
    if (peek() == Token::EOS) break;
    // Process directives.
    if (directive_prologue) {
      if (peek() == Token::STRING &&
          (scanner()->HasAnyLineTerminatorAfterNext() ||
           PeekAhead() == Token::SEMICOLON || PeekAhead() == end_token)) {
        Next();
        if (GetSymbol() == ast_value_factory()->use_strict_string())
          std::fprintf(stderr, "strict mode\n");  // !!!
        Check(Token::SEMICOLON);
        continue;
      } else {
        directive_prologue = false;
      }
    }
    // Process all the rest.
    ParseStmtOrDecl();
  }
}

void ParserSimple::ParseStmtOrDecl() {
  switch (peek()) {
    case Token::SEMICOLON:
      Next();
      break;
    case Token::FUNCTION:
      ParseExpressionList();
      break;
    case Token::CLASS:
      ParseClass();
      break;
    case Token::CONST:
    case Token::VAR:
      Next();
      ParseExpressionList();
      ExpectSemicolon();
      break;
    case Token::LET:
      if (IsNextLetKeyword()) Next();
      ParseExpressionList();
      ExpectSemicolon();
      break;
    case Token::LBRACE:
      ParseBlock();
      break;
    case Token::IF:
      ParseIfStatement();
      break;
    case Token::DO:
      ParseDoWhileStatement();
      break;
    case Token::WHILE:
      ParseWhileStatement();
      break;
    case Token::FOR:
      ParseForStatement();
      break;
    case Token::CONTINUE:
      ParseContinueStatement();
      break;
    case Token::BREAK:
      ParseBreakStatement();
      break;
    case Token::RETURN:
      ParseReturnStatement();
      break;
    case Token::THROW:
      ParseThrowStatement();
      break;
    case Token::TRY:
      ParseTryStatement();
      break;
    case Token::WITH:
      ParseWithStatement();
      break;
    case Token::SWITCH:
      ParseSwitchStatement();
      break;
    case Token::DEBUGGER:
      ParseDebuggerStatement();
      break;
    case Token::CASE:
    case Token::DEFAULT:
      ParseCaseClause();
      break;
    case Token::ASYNC:
      if (allow_harmony_async_await() && PeekAhead() == Token::FUNCTION &&
          !scanner()->HasAnyLineTerminatorAfterNext())
        Next();
      // Fall through to labeled statements and expressions.
    default:
      if (peek_any_identifier() && PeekAhead() == Token::COLON) {
        Next();
        Next();
      } else {
        ParseExpressionList();
        ExpectSemicolon();
      }
      break;
  }
}

void ParserSimple::ParseExpressionList() {
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
        ParseExpression();
        if (Check(Token::COMMA)) continue;
        break;
    }
    break;
  }
}

void ParserSimple::ParseExpression() {
  while (true) {
    ParseTerm();
    while (true) {
      Token::Value tok = peek();
      if (tok == Token::COMMA) return;
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
            ParseExpressionList();
            Expect(Token::RBRACK);
          }
          continue;
        case Token::LPAREN:
          Next();
          if (!Check(Token::RPAREN)) {
            ParseExpressionList();
            Expect(Token::RPAREN);
          }
          continue;
        case Token::PERIOD:
          Next();
          ParseIdentifierName();
          continue;
        case Token::TEMPLATE_SPAN:
        case Token::TEMPLATE_TAIL:
          ParseTemplateLiteral();
          continue;
        case Token::CONDITIONAL:
          Next();
          ParseExpression();
          Expect(Token::COLON);
          break;
        case Token::ARROW:
          ParseConciseBody();
          return;
        default:
          return;
      }
      break;
    }
  }
}

void ParserSimple::ParseTerm() {
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
      if (allow_harmony_async_await() &&
          !scanner()->HasAnyLineTerminatorAfterNext() &&
          PeekAhead() == Token::FUNCTION) {
        Next();
        ParseFunctionOrGenerator();
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
      ParseRegExpLiteral();
      break;
    case Token::LBRACK:
      ParseArrayLiteral();
      break;
    case Token::LBRACE:
      ParseObjectLiteral();
      break;
    case Token::LPAREN:
      Next();
      if (!Check(Token::RPAREN)) {
        ParseExpressionList();
        Expect(Token::RPAREN);
      }
      break;
    case Token::FUNCTION:
      if (allow_harmony_function_sent() && PeekAhead() == Token::PERIOD)
        Next();
      else
        ParseFunctionOrGenerator();
      break;
    case Token::CLASS:
      ParseClass();
      break;
    case Token::TEMPLATE_SPAN:
    case Token::TEMPLATE_TAIL:
      ParseTemplateLiteral();
      break;
    case Token::MOD:
      ParseV8Intrinsic();
      break;
    case Token::DO:
      ParseDoExpression();
      break;
    default:
      Error();
      break;
  }
}

void ParserSimple::ParseIdentifierName() {
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
      if (!Token::IsKeyword(tok)) Error();
      break;
  }
}

void ParserSimple::ParseRegExpLiteral() {
  DCHECK(peek() == Token::DIV || peek() == Token::ASSIGN_DIV);
  if (scanner()->ScanRegExpPattern(peek() == Token::ASSIGN_DIV)) {
    scanner()->ScanRegExpFlags();
    Next();
    return;
  }
  Error();
}

void ParserSimple::ParseTemplateLiteral() {
  DCHECK(peek() == Token::TEMPLATE_SPAN || peek() == Token::TEMPLATE_TAIL);
  if (Check(Token::TEMPLATE_TAIL)) return;
  Next();
  Token::Value next;
  do {
    next = peek();
    if (next == Token::EOS || next == Token::ILLEGAL) {
      Error();
      return;
    }
    ParseExpressionList();
    if (peek() != Token::RBRACE) {
      Error();
      return;
    }
    next = scanner()->ScanTemplateContinuation();
    Next();
    if (next == Token::EOS || next == Token::ILLEGAL) {
      Error();
      return;
    }
  } while (next == Token::TEMPLATE_SPAN);
  DCHECK_EQ(next, Token::TEMPLATE_TAIL);
}

void ParserSimple::ParseArrayLiteral() {
  DCHECK_EQ(peek(), Token::LBRACK);
  Next();
  if (!Check(Token::RBRACK)) {
    ParseExpressionList();
    Expect(Token::RBRACK);
  }
}

void ParserSimple::ParseObjectLiteral() {
  DCHECK_EQ(peek(), Token::LBRACE);
  Next();
  while (!Check(Token::RBRACE) && peek() != Token::EOS) {
    if (Check(Token::COMMA)) continue;
    ParsePropertyDefinition(false);
  }
}

void ParserSimple::ParseFunctionOrGenerator() {
  DCHECK_EQ(peek(), Token::FUNCTION);
  Next();
  bool generator = Check(Token::MUL);
  USE(generator);  // !!!
  if (peek_any_identifier())
    ParseIdentifierName();
  if (Check(Token::LPAREN)) {
    if (!Check(Token::RPAREN)) {
      ParseExpressionList();
      Expect(Token::RPAREN);
    }
    if (Check(Token::LBRACE)) {
      int body_start = scanner()->location().beg_pos;
      if (!Check(Token::RBRACE)) {
        ParseStmtOrDeclList(Token::RBRACE);
        Expect(Token::RBRACE);
      }
      std::fprintf(stderr, "simple, function boundaries: %d, %d\n",
                   body_start, scanner()->location().end_pos);
      return;
    }
  }
  Error();
}

void ParserSimple::ParseV8Intrinsic() {
  DCHECK_EQ(peek(), Token::MOD);
  Next();
  if (peek_any_identifier()) {
    ParseIdentifierName();
    if (Check(Token::LPAREN)) {
      if (!Check(Token::RPAREN)) {
        ParseExpressionList();
        Expect(Token::RPAREN);
      }
      return;
    }
  }
  Error();
}

void ParserSimple::ParseDoExpression() {
  DCHECK_EQ(peek(), Token::DO);
  Next();
  ParseBlock();
}

void ParserSimple::ParseClass() {
  DCHECK_EQ(peek(), Token::CLASS);
  Next();
  if (peek_any_identifier()) ParseIdentifierName();
  if (Check(Token::EXTENDS)) ParseExpression();
  if (Check(Token::LBRACE)) {
    while (!Check(Token::RBRACE) && peek() != Token::EOS) {
      if (Check(Token::SEMICOLON)) continue;
      ParsePropertyDefinition(true);
    }
    return;
  }
  Error();
}

void ParserSimple::ParsePropertyDefinition(bool in_class) {
  bool is_static = in_class && Check(Token::STATIC);
  bool is_generator = Check(Token::MUL);
  bool is_get = false;
  bool is_set = false;
  bool is_async =
      allow_harmony_async_await() && peek() == Token::ASYNC &&
      !scanner()->HasAnyLineTerminatorAfterNext() &&
      PeekAhead() != Token::LPAREN;
  ParsePropertyName(&is_get, &is_set);
  switch (peek()) {
    case Token::COLON:
    case Token::ASSIGN:
      Next();
      ParseExpression();
      return;
    case Token::COMMA:
    case Token::RBRACE:
    case Token::EOS:
      return;
    default:
      break;
  }
  if (is_get || is_set || (is_async && !is_generator && !is_static)) {
    bool dont_care;
    ParsePropertyName(&dont_care, &dont_care);
  }
  if (Check(Token::LPAREN)) {
    if (!Check(Token::RPAREN)) {
      ParseExpressionList();
      Expect(Token::RPAREN);
    }
    if (Check(Token::LBRACE)) {
      int body_start = scanner()->location().beg_pos;
      if (!Check(Token::RBRACE)) {
        ParseStmtOrDeclList(Token::RBRACE);
        Expect(Token::RBRACE);
      }
      std::fprintf(stderr, "simple, method boundaries: %d, %d\n",
                   body_start, scanner()->location().end_pos);
      return;
    }
  }
  Error();
}

void ParserSimple::ParsePropertyName(bool* is_get, bool* is_set) {
  switch (peek()) {
    case Token::STRING:
    case Token::SMI:
    case Token::NUMBER:
      Next();
      break;
    case Token::LBRACK:
      Next();
      ParseExpressionList();
      Expect(Token::RBRACK);
      break;
    default:
      ParseIdentifierName();
      scanner()->IsGetOrSet(is_get, is_set);
      break;
  }
}

void ParserSimple::ParseBlock() {
  if (Check(Token::LBRACE)) {
    if (!Check(Token::RBRACE)) {
      ParseStmtOrDeclList(Token::RBRACE);
      Expect(Token::RBRACE);
    }
    return;
  }
  Error();
}

void ParserSimple::ParseIfStatement() {
  DCHECK_EQ(peek(), Token::IF);
  Next();
  if (Check(Token::LPAREN)) {
    ParseExpressionList();
    if (Check(Token::RPAREN)) {
      ParseStmtOrDecl();
      if (Check(Token::ELSE)) ParseStmtOrDecl();
      return;
    }
  }
  Error();
}

void ParserSimple::ParseDoWhileStatement() {
  DCHECK_EQ(peek(), Token::DO);
  Next();
  ParseStmtOrDecl();
  if (Check(Token::WHILE) && Check(Token::LPAREN)) {
    ParseExpressionList();
    Expect(Token::RPAREN);
    Check(Token::SEMICOLON);
    return;
  }
  Error();
}

void ParserSimple::ParseWhileStatement() {
  DCHECK_EQ(peek(), Token::WHILE);
  Next();
  if (Check(Token::LPAREN)) {
    ParseExpressionList();
    if (Check(Token::RPAREN)) {
      ParseStmtOrDecl();
      return;
    }
  }
  Error();
}

void ParserSimple::ParseForStatement() {
  DCHECK_EQ(peek(), Token::FOR);
  Next();
  if (Check(Token::LPAREN)) {
    while (peek() != Token::RPAREN && peek() != Token::EOS) {
      switch (peek()) {
        case Token::SEMICOLON:
          Next();
          continue;
        case Token::CONST:
        case Token::VAR:
          Next();
          ParseExpressionList();
          break;
        case Token::LET:
          if (IsNextLetKeyword()) Next();
          ParseExpressionList();
          break;
        default:
          ParseExpressionList();
      }
      if (PeekContextualKeyword("of")) {
        Next();
        ParseExpressionList();
      }
    }
    if (Check(Token::RPAREN)) {
      ParseStmtOrDecl();
      return;
    }
  }
  Error();
}

void ParserSimple::ParseWithStatement() {
  DCHECK_EQ(peek(), Token::WITH);
  Next();
  if (Check(Token::LPAREN)) {
    ParseExpressionList();
    if (Check(Token::RPAREN)) {
      ParseStmtOrDecl();
      return;
    }
  }
  Error();
}

void ParserSimple::ParseSwitchStatement() {
  DCHECK_EQ(peek(), Token::SWITCH);
  Next();
  if (Check(Token::LPAREN)) {
    ParseExpressionList();
    if (Check(Token::RPAREN)) {
      ParseBlock();
      return;
    }
  }
  Error();
}

void ParserSimple::ParseCaseClause() {
  DCHECK(peek() == Token::CASE || peek() == Token::DEFAULT);
  if (Check(Token::CASE))
    ParseExpressionList();
  else
    Next();
  Expect(Token::COLON);
}

void ParserSimple::ParseDebuggerStatement() {
  DCHECK_EQ(peek(), Token::DEBUGGER);
  Next();
  ExpectSemicolon();
}

void ParserSimple::ParseContinueStatement() {
  DCHECK_EQ(peek(), Token::CONTINUE);
  Next();
  Token::Value tok = peek();
  if (!scanner()->HasAnyLineTerminatorBeforeNext() &&
      tok != Token::SEMICOLON && tok != Token::RBRACE && tok != Token::EOS) {
    ParseIdentifierName();
  }
  ExpectSemicolon();
}

void ParserSimple::ParseBreakStatement() {
  DCHECK_EQ(peek(), Token::BREAK);
  Next();
  Token::Value tok = peek();
  if (!scanner()->HasAnyLineTerminatorBeforeNext() &&
      tok != Token::SEMICOLON && tok != Token::RBRACE && tok != Token::EOS)
    ParseIdentifierName();
  ExpectSemicolon();
}

void ParserSimple::ParseReturnStatement() {
  DCHECK_EQ(peek(), Token::RETURN);
  Next();
  Token::Value tok = peek();
  if (!scanner()->HasAnyLineTerminatorBeforeNext() &&
      tok != Token::SEMICOLON && tok != Token::RBRACE && tok != Token::EOS)
    ParseExpressionList();
  ExpectSemicolon();
}

void ParserSimple::ParseThrowStatement() {
  DCHECK_EQ(peek(), Token::THROW);
  Next();
  ParseExpressionList();
  ExpectSemicolon();
}

void ParserSimple::ParseTryStatement() {
  DCHECK_EQ(peek(), Token::TRY);
  Next();
  ParseBlock();
  if (Check(Token::CATCH) && Check(Token::LPAREN)) {
    ParseExpressionList();
    Expect(Token::RPAREN);
    ParseBlock();
  }
  if (Check(Token::FINALLY)) ParseBlock();
}

void ParserSimple::ParseConciseBody() {
  DCHECK_EQ(peek(), Token::ARROW);
  Next();
  int body_start = scanner()->location().beg_pos;
  if (peek() == Token::LBRACE)
    ParseBlock();
  else
    ParseExpression();
  std::fprintf(stderr, "simple, arrow function boundaries: %d, %d\n",
               body_start, scanner()->location().end_pos);
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

void ParserSimple::Error() {
  std::fprintf(stderr, "Parsing simple: error at %d, ignoring token %s\n",
               scanner()->peek_location().beg_pos, Token::Name(peek()));
  Next();
}

void ParserSimple::Expect(Token::Value token) {
  if (!Check(token)) Error();
}

void ParserSimple::ExpectSemicolon() {
  while (true) {
    Token::Value tok = peek();
    switch (tok) {
      case Token::SEMICOLON:
        Next();
        // Fall through.
      case Token::RBRACE:
      case Token::EOS:
        return;
      default:
        if (scanner()->HasAnyLineTerminatorBeforeNext())
          return;
        break;
    }
    Error();
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
