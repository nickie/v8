// Copyright 2015 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#include "src/ast/ast.h"
#include "src/ast/ast-expression-rewriter.h"

namespace v8 {
namespace internal {

// ----------------------------------------------------------------------------
// Implementation of AstExpressionRewriter
// The AST is traversed but no actual rewriting takes place, unless the
// Visit methods are overriden in subclasses.

#define REWRITE_THIS(node) do {                 \
    if (!RewriteExpression(node)) return;       \
  } while(false)
#define NOTHING() DCHECK_NULL(replacement_)


void AstExpressionRewriter::VisitDeclarations(
    ZoneList<Declaration*>* declarations) {
  for (int i = 0; i < declarations->length(); i++) {
    AST_REWRITE(Declaration, declarations->at(i),
                declarations->Set(i, replacement));
  }
}


void AstExpressionRewriter::VisitStatements(ZoneList<Statement*>* statements) {
  for (int i = 0; i < statements->length(); i++) {
    AST_REWRITE(Statement, statements->at(i), statements->Set(i, replacement));
    // Not stopping when a jump statement is found.
  }
}


void AstExpressionRewriter::VisitExpressions(
    ZoneList<Expression*>* expressions) {
  for (int i = 0; i < expressions->length(); i++) {
    // The variable statement visiting code may pass NULL expressions
    // to this code. Maybe this should be handled by introducing an
    // undefined expression or literal?  Revisit this code if this
    // changes
    Expression* expression = expressions->at(i);
    if (expression != nullptr) {
      AST_REWRITE(Expression, expression, expressions->Set(i, replacement));
    }
  }
}


void AstExpressionRewriter::VisitVariableDeclaration(VariableDeclaration* node) {
  // Not visiting `proxy_`.
  NOTHING();
}


void AstExpressionRewriter::VisitFunctionDeclaration(FunctionDeclaration* node) {
  // Not visiting `proxy_`.
  AST_REWRITE(FunctionLiteral, node->fun(), node->set_fun(replacement));
}


void AstExpressionRewriter::VisitImportDeclaration(ImportDeclaration* node) {
  // Not visiting `proxy_`.
  NOTHING();
}


void AstExpressionRewriter::VisitExportDeclaration(ExportDeclaration* node) {
  // Not visiting `proxy_`.
  NOTHING();
}


void AstExpressionRewriter::VisitBlock(Block* node) {
  VisitStatements(node->statements());
}


void AstExpressionRewriter::VisitExpressionStatement(ExpressionStatement* node) {
  AST_REWRITE(Expression, node->expression(),
              node->set_expression(replacement));
}


void AstExpressionRewriter::VisitEmptyStatement(EmptyStatement* node) {
  NOTHING();
}


void AstExpressionRewriter::VisitSloppyBlockFunctionStatement(
    SloppyBlockFunctionStatement* node) {
  AST_REWRITE(Statement, node->statement(), node->set_statement(replacement));
}


void AstExpressionRewriter::VisitIfStatement(IfStatement* node) {
  AST_REWRITE(Expression, node->condition(), node->set_condition(replacement));
  AST_REWRITE(Statement, node->then_statement(),
              node->set_then_statement(replacement));
  AST_REWRITE(Statement, node->else_statement(),
              node->set_else_statement(replacement));
}


void AstExpressionRewriter::VisitContinueStatement(ContinueStatement* node) {
  NOTHING();
}


void AstExpressionRewriter::VisitBreakStatement(BreakStatement* node) {
  NOTHING();
}


void AstExpressionRewriter::VisitReturnStatement(ReturnStatement* node) {
  AST_REWRITE(Expression, node->expression(),
              node->set_expression(replacement));
}


void AstExpressionRewriter::VisitWithStatement(WithStatement* node) {
  AST_REWRITE(Expression, node->expression(),
              node->set_expression(replacement));
  AST_REWRITE(Statement, node->statement(), node->set_statement(replacement));
}


void AstExpressionRewriter::VisitSwitchStatement(SwitchStatement* node) {
  AST_REWRITE(Expression, node->tag(), node->set_tag(replacement));
  ZoneList<CaseClause*>* clauses = node->cases();
  for (int i = 0; i < clauses->length(); i++) {
    AST_REWRITE(CaseClause, clauses->at(i), clauses->Set(i, replacement));
  }
}


void AstExpressionRewriter::VisitDoWhileStatement(DoWhileStatement* node) {
  AST_REWRITE(Expression, node->cond(), node->set_cond(replacement));
  AST_REWRITE(Statement, node->body(), node->set_body(replacement));
}


void AstExpressionRewriter::VisitWhileStatement(WhileStatement* node) {
  AST_REWRITE(Expression, node->cond(), node->set_cond(replacement));
  AST_REWRITE(Statement, node->body(), node->set_body(replacement));
}


void AstExpressionRewriter::VisitForStatement(ForStatement* node) {
  if (node->init() != nullptr) {
    AST_REWRITE(Statement, node->init(), node->set_init(replacement));
  }
  if (node->cond() != nullptr) {
    AST_REWRITE(Expression, node->cond(), node->set_cond(replacement));
  }
  if (node->next() != nullptr) {
    AST_REWRITE(Statement, node->next(), node->set_next(replacement));
  }
  AST_REWRITE(Statement, node->body(), node->set_body(replacement));
}


void AstExpressionRewriter::VisitForInStatement(ForInStatement* node) {
  AST_REWRITE(Expression, node->each(), node->set_each(replacement));
  AST_REWRITE(Expression, node->subject(), node->set_subject(replacement));
  AST_REWRITE(Statement, node->body(), node->set_body(replacement));
}


void AstExpressionRewriter::VisitForOfStatement(ForOfStatement* node) {
  AST_REWRITE(Expression, node->each(), node->set_each(replacement));
  AST_REWRITE(Expression, node->subject(), node->set_subject(replacement));
  AST_REWRITE(Statement, node->body(), node->set_body(replacement));
  // Not visiting `assign_iterator_`, `next_result_`, `result_done_`
  // and `assign_each_`.
}


void AstExpressionRewriter::VisitTryCatchStatement(TryCatchStatement* node) {
  AST_REWRITE(Block, node->try_block(), node->set_try_block(replacement));
  // Not visiting the variable.
  AST_REWRITE(Block, node->catch_block(), node->set_catch_block(replacement));
}


void AstExpressionRewriter::VisitTryFinallyStatement(TryFinallyStatement* node) {
  AST_REWRITE(Block, node->try_block(), node->set_try_block(replacement));
  AST_REWRITE(Block, node->finally_block(),
              node->set_finally_block(replacement));
}


void AstExpressionRewriter::VisitDebuggerStatement(DebuggerStatement* node) {
  NOTHING();
}


void AstExpressionRewriter::VisitFunctionLiteral(FunctionLiteral* node) {
  REWRITE_THIS(node);
  VisitDeclarations(node->scope()->declarations());
  ZoneList<Statement*>* body = node->body();
  if (body != nullptr) VisitStatements(body);
}


void AstExpressionRewriter::VisitClassLiteral(ClassLiteral* node) {
  REWRITE_THIS(node);
  // Not visiting `class_variable_proxy_`.
  Expression* extends = node->extends();
  if (extends != nullptr) {
    AST_REWRITE(Expression, extends, node->set_extends(replacement));
  }
  AST_REWRITE(FunctionLiteral, node->constructor(),
              node->set_constructor(replacement));
  ZoneList<typename ClassLiteral::Property*>* properties = node->properties();
  for (int i = 0; i < properties->length(); i++) {
    VisitObjectLiteralProperty(properties->at(i));
  }
}


void AstExpressionRewriter::VisitNativeFunctionLiteral(
    NativeFunctionLiteral* node) {
  REWRITE_THIS(node);
  NOTHING();
}


void AstExpressionRewriter::VisitConditional(Conditional* node) {
  REWRITE_THIS(node);
  AST_REWRITE(Expression, node->condition(), node->set_condition(replacement));
  AST_REWRITE(Expression, node->then_expression(),
          node->set_then_expression(replacement));
  AST_REWRITE(Expression, node->else_expression(),
          node->set_else_expression(replacement));
}


void AstExpressionRewriter::VisitVariableProxy(VariableProxy* node) {
  REWRITE_THIS(node);
  NOTHING();
}


void AstExpressionRewriter::VisitLiteral(Literal* node) {
  REWRITE_THIS(node);
  NOTHING();
}


void AstExpressionRewriter::VisitRegExpLiteral(RegExpLiteral* node) {
  REWRITE_THIS(node);
  NOTHING();
}


void AstExpressionRewriter::VisitObjectLiteral(ObjectLiteral* node) {
  REWRITE_THIS(node);
  ZoneList<typename ObjectLiteral::Property*>* properties = node->properties();
  for (int i = 0; i < properties->length(); i++) {
    VisitObjectLiteralProperty(properties->at(i));
  }
}


void AstExpressionRewriter::VisitObjectLiteralProperty(
    ObjectLiteralProperty* property) {
  if (property == nullptr) return;
  AST_REWRITE(Expression, property->key(), property->set_key(replacement));
  AST_REWRITE(Expression, property->value(), property->set_value(replacement));
}


void AstExpressionRewriter::VisitArrayLiteral(ArrayLiteral* node) {
  REWRITE_THIS(node);
  VisitExpressions(node->values());
}


void AstExpressionRewriter::VisitAssignment(Assignment* node) {
  REWRITE_THIS(node);
  AST_REWRITE(Expression, node->target(), node->set_target(replacement));
  AST_REWRITE(Expression, node->value(), node->set_value(replacement));
}


void AstExpressionRewriter::VisitYield(Yield* node) {
  REWRITE_THIS(node);
  AST_REWRITE(Expression, node->generator_object(),
          node->set_generator_object(replacement));
  AST_REWRITE(Expression, node->expression(),
          node->set_expression(replacement));
}


void AstExpressionRewriter::VisitThrow(Throw* node) {
  REWRITE_THIS(node);
  AST_REWRITE(Expression, node->exception(), node->set_exception(replacement));
}


void AstExpressionRewriter::VisitProperty(Property* node) {
  REWRITE_THIS(node);
  if (node == nullptr) return;
  AST_REWRITE(Expression, node->obj(), node->set_obj(replacement));
  AST_REWRITE(Expression, node->key(), node->set_key(replacement));
}


void AstExpressionRewriter::VisitCall(Call* node) {
  REWRITE_THIS(node);
  AST_REWRITE(Expression, node->expression(),
              node->set_expression(replacement));
  VisitExpressions(node->arguments());
}


void AstExpressionRewriter::VisitCallNew(CallNew* node) {
  REWRITE_THIS(node);
  AST_REWRITE(Expression, node->expression(),
              node->set_expression(replacement));
  VisitExpressions(node->arguments());
}


void AstExpressionRewriter::VisitCallRuntime(CallRuntime* node) {
  REWRITE_THIS(node);
  VisitExpressions(node->arguments());
}


void AstExpressionRewriter::VisitUnaryOperation(UnaryOperation* node) {
  REWRITE_THIS(node);
  AST_REWRITE(Expression, node->expression(),
              node->set_expression(replacement));
}


void AstExpressionRewriter::VisitCountOperation(CountOperation* node) {
  REWRITE_THIS(node);
  AST_REWRITE(Expression, node->expression(),
              node->set_expression(replacement));
}


void AstExpressionRewriter::VisitBinaryOperation(BinaryOperation* node) {
  REWRITE_THIS(node);
  AST_REWRITE(Expression, node->left(), node->set_left(replacement));
  AST_REWRITE(Expression, node->right(), node->set_right(replacement));
}


void AstExpressionRewriter::VisitCompareOperation(CompareOperation* node) {
  REWRITE_THIS(node);
  AST_REWRITE(Expression, node->left(), node->set_left(replacement));
  AST_REWRITE(Expression, node->right(), node->set_right(replacement));
}


void AstExpressionRewriter::VisitSpread(Spread* node) {
  REWRITE_THIS(node);
  AST_REWRITE(Expression, node->expression(),
              node->set_expression(replacement));
}


void AstExpressionRewriter::VisitThisFunction(ThisFunction* node) {
  REWRITE_THIS(node);
  NOTHING();
}


void AstExpressionRewriter::VisitSuperPropertyReference(
    SuperPropertyReference* node) {
  REWRITE_THIS(node);
  AST_REWRITE(VariableProxy, node->this_var(), node->set_this_var(replacement));
  AST_REWRITE(Expression, node->home_object(),
              node->set_home_object(replacement));
}


void AstExpressionRewriter::VisitSuperCallReference(SuperCallReference* node) {
  REWRITE_THIS(node);
  AST_REWRITE(VariableProxy, node->this_var(), node->set_this_var(replacement));
  AST_REWRITE(VariableProxy, node->new_target_var(),
              node->set_new_target_var(replacement));
  AST_REWRITE(VariableProxy, node->this_function_var(),
              node->set_this_function_var(replacement));
}


void AstExpressionRewriter::VisitCaseClause(CaseClause* node) {
  if (!node->is_default()) {
    AST_REWRITE(Expression, node->label(), node->set_label(replacement));
  }
  VisitStatements(node->statements());
}


void AstExpressionRewriter::VisitEmptyParentheses(EmptyParentheses* node) {
  NOTHING();
}


void AstExpressionRewriter::VisitDoExpression(DoExpression* node) {
  REWRITE_THIS(node);
  AST_REWRITE(Block, node->block(), node->set_block(replacement));
  AST_REWRITE(VariableProxy, node->result(), node->set_result(replacement));
}


void AstExpressionRewriter::VisitRewritableAssignmentExpression(
    RewritableAssignmentExpression* node) {
  REWRITE_THIS(node);
  AST_REWRITE(Expression, node->expression(),
              node->set_expression(replacement));
}


}  // namespace internal
}  // namespace v8
