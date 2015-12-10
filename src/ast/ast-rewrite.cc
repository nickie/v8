// Copyright 2015 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#include "src/ast/ast.h"

namespace v8 {
namespace internal {

// ----------------------------------------------------------------------------
// Implementation of AstRewriter
// The AST is traversed but no actual rewriting takes place, unless the
// Visit methods are overriden in subclasses.


// Macro for rewriting things; `CMD` is the command that should
// do something sensible with the variable called `replacement`.
#define REWRITE(Type, GET, SET)                                     \
  do {                                                              \
    DCHECK_NULL(replacement_);                                      \
    (GET)->Accept(this);                                            \
    if (replacement_ != nullptr) {                                  \
      Type* replacement = reinterpret_cast<Type*>(replacement_);    \
      do { SET; } while(0);                                         \
      replacement_ = nullptr;                                       \
    }                                                               \
  } while(0)


// Macro for not rewriting anything; the invariant is that
// `replacement_` should be kept null after updating.
#define NOTHING()                                 \
  do {                                            \
    DCHECK_NULL(replacement_);                    \
  } while(0)


void AstRewriter::VisitDeclarations(ZoneList<Declaration*>* declarations) {
  for (int i = 0; i < declarations->length(); i++) {
    REWRITE(Declaration, declarations->at(i),
            declarations->Set(i, replacement));
  }
}


void AstRewriter::VisitStatements(ZoneList<Statement*>* statements) {
  for (int i = 0; i < statements->length(); i++) {
    REWRITE(Statement, statements->at(i), statements->Set(i, replacement));
  }
}


void AstRewriter::VisitExpressions(ZoneList<Expression*>* expressions) {
  for (int i = 0; i < expressions->length(); i++) {
    // The variable statement visiting code may pass NULL expressions
    // to this code. Maybe this should be handled by introducing an
    // undefined expression or literal?  Revisit this code if this
    // changes
    Expression* expression = expressions->at(i);
    if (expression != NULL) {
      REWRITE(Expression, expression, expressions->Set(i, replacement));
    }
  }
}


void AstRewriter::VisitVariableDeclaration(VariableDeclaration* node) {
  // Not visiting `proxy_`.
  NOTHING();
}


void AstRewriter::VisitFunctionDeclaration(FunctionDeclaration* node) {
  // Not visiting `proxy_`.
  REWRITE(FunctionLiteral, node->fun(), node->set_fun(replacement));
}


void AstRewriter::VisitImportDeclaration(ImportDeclaration* node) {
  // Not visiting `proxy_`.
  NOTHING();
}


void AstRewriter::VisitExportDeclaration(ExportDeclaration* node) {
  // Not visiting `proxy_`.
  NOTHING();
}


void AstRewriter::VisitBlock(Block* node) {
  VisitStatements(node->statements());
}


void AstRewriter::VisitExpressionStatement(ExpressionStatement* node) {
  REWRITE(Expression, node->expression(), node->set_expression(replacement));
}


void AstRewriter::VisitEmptyStatement(EmptyStatement* node) {
  NOTHING();
}


void AstRewriter::VisitSloppyBlockFunctionStatement(
    SloppyBlockFunctionStatement* node) {
  REWRITE(Statement, node->statement(), node->set_statement(replacement));
}


void AstRewriter::VisitIfStatement(IfStatement* node) {
  REWRITE(Expression, node->condition(), node->set_condition(replacement));
  REWRITE(Statement, node->then_statement(),
          node->set_then_statement(replacement));
  REWRITE(Statement, node->else_statement(),
          node->set_else_statement(replacement));
}


void AstRewriter::VisitContinueStatement(ContinueStatement* node) {
  NOTHING();
}


void AstRewriter::VisitBreakStatement(BreakStatement* node) {
  NOTHING();
}


void AstRewriter::VisitReturnStatement(ReturnStatement* node) {
  REWRITE(Expression, node->expression(), node->set_expression(replacement));
}


void AstRewriter::VisitWithStatement(WithStatement* node) {
  REWRITE(Expression, node->expression(), node->set_expression(replacement));
  REWRITE(Statement, node->statement(), node->set_statement(replacement));
}


void AstRewriter::VisitSwitchStatement(SwitchStatement* node) {
  REWRITE(Expression, node->tag(), node->set_tag(replacement));
  ZoneList<CaseClause*>* clauses = node->cases();
  for (int i = 0; i < clauses->length(); i++) {
    REWRITE(CaseClause, clauses->at(i), clauses->Set(i, replacement));
  }
}


void AstRewriter::VisitDoWhileStatement(DoWhileStatement* node) {
  REWRITE(Expression, node->cond(), node->set_cond(replacement));
  REWRITE(Statement, node->body(), node->set_body(replacement));
}


void AstRewriter::VisitWhileStatement(WhileStatement* node) {
  REWRITE(Expression, node->cond(), node->set_cond(replacement));
  REWRITE(Statement, node->body(), node->set_body(replacement));
}


void AstRewriter::VisitForStatement(ForStatement* node) {
  REWRITE(Statement, node->init(), node->set_init(replacement));
  REWRITE(Expression, node->cond(), node->set_cond(replacement));
  REWRITE(Statement, node->next(), node->set_next(replacement));
  REWRITE(Statement, node->body(), node->set_body(replacement));
}


void AstRewriter::VisitForInStatement(ForInStatement* node) {
  REWRITE(Expression, node->each(), node->set_each(replacement));
  REWRITE(Expression, node->subject(), node->set_subject(replacement));
  REWRITE(Statement, node->body(), node->set_body(replacement));
}


void AstRewriter::VisitForOfStatement(ForOfStatement* node) {
  REWRITE(Expression, node->each(), node->set_each(replacement));
  REWRITE(Expression, node->subject(), node->set_subject(replacement));
  REWRITE(Statement, node->body(), node->set_body(replacement));
  // Not visiting `assign_iterator_`, `next_result_`, `result_done_`
  // and `assign_each_`.
}


void AstRewriter::VisitTryCatchStatement(TryCatchStatement* node) {
  REWRITE(Block, node->try_block(), node->set_try_block(replacement));
  // Not visiting the variable.
  REWRITE(Block, node->catch_block(), node->set_catch_block(replacement));
}


void AstRewriter::VisitTryFinallyStatement(TryFinallyStatement* node) {
  REWRITE(Block, node->try_block(), node->set_try_block(replacement));
  REWRITE(Block, node->finally_block(), node->set_finally_block(replacement));
}


void AstRewriter::VisitDebuggerStatement(DebuggerStatement* node) {
  NOTHING();
}


void AstRewriter::VisitFunctionLiteral(FunctionLiteral* node) {
  VisitStatements(node->body());
}


void AstRewriter::VisitClassLiteral(ClassLiteral* node) {
  // Not visiting `class_variable_proxy_`.
  REWRITE(Expression, node->extends(), node->set_extends(replacement));
  REWRITE(FunctionLiteral, node->constructor(),
          node->set_constructor(replacement));
  ZoneList<typename ClassLiteral::Property*>* properties = node->properties();
  for (int i = 0; i < properties->length(); i++) {
    typename ClassLiteral::Property* property = properties->at(i);
    if (property == nullptr) continue;
    REWRITE(Expression, property->key(), property->set_key(replacement));
    REWRITE(Expression, property->value(), property->set_value(replacement));
  }
}


void AstRewriter::VisitNativeFunctionLiteral(NativeFunctionLiteral* node) {
  NOTHING();
}


void AstRewriter::VisitConditional(Conditional* node) {
  REWRITE(Expression, node->condition(), node->set_condition(replacement));
  REWRITE(Expression, node->then_expression(),
          node->set_then_expression(replacement));
  REWRITE(Expression, node->else_expression(),
          node->set_else_expression(replacement));
}


void AstRewriter::VisitVariableProxy(VariableProxy* node) {
  NOTHING();
}


void AstRewriter::VisitLiteral(Literal* node) {
  NOTHING();
}


void AstRewriter::VisitRegExpLiteral(RegExpLiteral* node) {
  NOTHING();
}


void AstRewriter::VisitObjectLiteral(ObjectLiteral* node) {
  ZoneList<typename ObjectLiteral::Property*>* properties = node->properties();
  for (int i = 0; i < properties->length(); i++) {
    typename ObjectLiteral::Property* property = properties->at(i);
    if (property == nullptr) continue;
    REWRITE(Expression, property->key(), property->set_key(replacement));
    REWRITE(Expression, property->value(), property->set_value(replacement));
  }
}


void AstRewriter::VisitArrayLiteral(ArrayLiteral* node) {
  VisitExpressions(node->values());
}


void AstRewriter::VisitAssignment(Assignment* node) {
  REWRITE(Expression, node->target(), node->set_target(replacement));
  REWRITE(Expression, node->value(), node->set_value(replacement));
}


void AstRewriter::VisitYield(Yield* node) {
  REWRITE(Expression, node->generator_object(),
          node->set_generator_object(replacement));
  REWRITE(Expression, node->expression(),
          node->set_expression(replacement));
}


void AstRewriter::VisitThrow(Throw* node) {
  REWRITE(Expression, node->exception(), node->set_exception(replacement));
}


void AstRewriter::VisitProperty(Property* node) {
  if (node == nullptr) return;
  REWRITE(Expression, node->obj(), node->set_obj(replacement));
  REWRITE(Expression, node->key(), node->set_key(replacement));
}


void AstRewriter::VisitCall(Call* node) {
  REWRITE(Expression, node->expression(), node->set_expression(replacement));
  VisitExpressions(node->arguments());
}


void AstRewriter::VisitCallNew(CallNew* node) {
  REWRITE(Expression, node->expression(), node->set_expression(replacement));
  VisitExpressions(node->arguments());
}


void AstRewriter::VisitCallRuntime(CallRuntime* node) {
  VisitExpressions(node->arguments());
}


void AstRewriter::VisitUnaryOperation(UnaryOperation* node) {
  REWRITE(Expression, node->expression(), node->set_expression(replacement));
}


void AstRewriter::VisitCountOperation(CountOperation* node) {
  REWRITE(Expression, node->expression(), node->set_expression(replacement));
}


void AstRewriter::VisitBinaryOperation(BinaryOperation* node) {
  REWRITE(Expression, node->left(), node->set_left(replacement));
  REWRITE(Expression, node->right(), node->set_right(replacement));
}


void AstRewriter::VisitCompareOperation(CompareOperation* node) {
  REWRITE(Expression, node->left(), node->set_left(replacement));
  REWRITE(Expression, node->right(), node->set_right(replacement));
}


void AstRewriter::VisitSpread(Spread* node) {
  REWRITE(Expression, node->expression(), node->set_expression(replacement));
}


void AstRewriter::VisitThisFunction(ThisFunction* node) {
  NOTHING();
}


void AstRewriter::VisitSuperPropertyReference(SuperPropertyReference* node) {
  REWRITE(Expression, node->home_object(), node->set_home_object(replacement));
}


void AstRewriter::VisitSuperCallReference(SuperCallReference* node) {
  NOTHING();
}


void AstRewriter::VisitCaseClause(CaseClause* node) {
  if (!node->is_default()) {
    REWRITE(Expression, node->label(), node->set_label(replacement));
  }
  VisitStatements(node->statements());
}


void AstRewriter::VisitEmptyParentheses(EmptyParentheses* node) {
  NOTHING();
}


void AstRewriter::VisitDoExpression(DoExpression* node) {
  REWRITE(Block, node->block(), node->set_block(replacement));
  // Not visiting `result_`.
}


void AstRewriter::VisitRewritableAssignmentExpression(
    RewritableAssignmentExpression* node) {
  REWRITE(Expression, node->expression(), node->set_expression(replacement));
}


}  // namespace internal
}  // namespace v8
