// Compile with -std=gnu99

#include <assert.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <math.h>

// we need `assert()` to always do the assersions
#undef NDEBUG

// == Lexer ==

typedef struct {
  char *cursor;
} Lexer;

typedef enum {
  TK_NULL, // means end of input and used for missing token
  TK_SPACE,
  TK_NUMBER,
  TK_IDENT,
  TK_ADD,
  TK_SUB,
  TK_MUL,
  TK_DIV,
  TK_POW,
  TK_OPEN_PAREN,
  TK_CLOSED_PAREN
} TokenKind;

typedef struct {
  TokenKind kind;
  // Only some tokens will have text
  char *text;
  size_t text_len;
} Token;

// returns length of the lexed token, or 0
int lex_space(char *s) {
  int i = 0;
  while (s[i] == ' ' || s[i] == '\n') {
    i++;
  }
  return i;
}

bool is_digit(char ch) { return ch >= '0' && ch <= '9'; }

// returns length of the lexed token, or 0
int lex_number(char *s) {
  int i = 0;
  while (is_digit(s[i]) || s[i] == '.') {
    i++;
  }
  return i;
}

// returns length of the lexed token, or 0
int lex_ident(char *s) {
  // identifiers cannot start with a number
  if (is_digit(s[0])) {
    return 0;
  }
  int i = 0;
  while ((s[i] >= 'a' && s[i] <= 'z') || (s[i] >= 'A' && s[i] <= 'Z') ||
         is_digit(s[i]) || s[i] == '_') {
    i++;
  }
  return i;
}

// returns TK_NULL, if `ch` is not an operator
TokenKind is_operator(char ch) {
  switch (ch) {
  case '+':
    return TK_ADD;
  case '-':
    return TK_SUB;
  case '*':
    return TK_MUL;
  case '/':
    return TK_DIV;
  case '^':
    return TK_POW;
  case '(':
    return TK_OPEN_PAREN;
  case ')':
    return TK_CLOSED_PAREN;
  default:
    return TK_NULL;
  }
}

Token lexer_next(Lexer *l) {
  if (l->cursor[0] == '\0') {
    return (Token){.kind = TK_NULL};
  }

  int space_len = lex_space(l->cursor);
  if (space_len) {
    l->cursor += space_len;
    return (Token){.kind = TK_SPACE};
  }

  int number_len = lex_number(l->cursor);
  if (number_len) {
    char *text = l->cursor;
    l->cursor += number_len;
    return (Token){.kind = TK_NUMBER, .text = text, .text_len = number_len};
  }

  int ident_len = lex_ident(l->cursor);
  if (ident_len) {
    char *text = l->cursor;
    l->cursor += ident_len;
    return (Token){.kind = TK_IDENT, .text = text, .text_len = ident_len};
  }

  TokenKind op = is_operator(l->cursor[0]);
  if (op != TK_NULL) {
    l->cursor += 1;
    return (Token){.kind = op};
  }

  assert(false && "Unknown Token");
}

Token lexer_next_nonspace(Lexer *l) {
  Token next = lexer_next(l);
  while (next.kind == TK_SPACE) {
    next = lexer_next(l);
  }
  return next;
}

// == Parser ==

typedef enum {
  // different kinds of expressions have different `data`
  EXPR_ADD,    // has data.binary
  EXPR_SUB,    // has data.binary
  EXPR_MUL,    // has data.binary
  EXPR_DIV,    // has data.binary
  EXPR_POW,    // has data.binary
  EXPR_NEG,    // has data.unary
  EXPR_NUMBER, // has data.number
  EXPR_VAR,    // has data.var
  EXPR_FUNC,   // has data.func
} ExprKind;

typedef size_t ExprIndex; // index of an Expr in parser.exprs

typedef struct {
  ExprIndex left;
  ExprIndex right;
} BinaryExpr;

typedef struct {
  char *name;
  size_t name_len;
  ExprIndex argument;
} FunctionExpr;

typedef struct {
  char *name;
  size_t name_len;
} VarExpr;

// union of all the data and Expr can hold
// The `kind` of an Expr determines which field of the union to use
typedef union {
  ExprIndex unary_op;
  BinaryExpr binary;
  FunctionExpr func;
  VarExpr var;
  long double number;
} ExprData;

// Tagged union representing a node in the AST
typedef struct {
  ExprKind kind;
  ExprData data;
} Expr;

typedef struct {
  Lexer lexer;
  Token lookahead;
  // dynamic array of all the Exprs in the parsed AST
  Expr *exprs;
  size_t exprs_len;
  size_t exprs_cap;
} Parser;

// returns TK_NULL if failed to eat token of kind `tk`
Token parser_try_eat(Parser *p, TokenKind tk) {
  if (p->lookahead.kind == tk) {
    Token token = p->lookahead;
    p->lookahead = lexer_next_nonspace(&p->lexer);
    return token;
  } else {
    return (Token){.kind = TK_NULL};
  }
}
Token parser_eat(Parser *p, TokenKind tk) {
  Token token = parser_try_eat(p, tk);
  if (token.kind == TK_NULL) {
    fprintf(stderr, "Expected token of kind %d\n", tk);
    assert(false);
  } else {
    return token;
  }
}

// pushes new expr into array of exprs and returnes its index
ExprIndex parser_push_expr(Parser *p, Expr new_expr) {
  if (p->exprs_len == p->exprs_cap) {
    size_t new_cap = p->exprs_cap * 2;
    p->exprs = realloc(p->exprs, sizeof(Expr) * new_cap);
    p->exprs_cap = new_cap;
  }

  p->exprs[p->exprs_len] = new_expr;
  p->exprs_len += 1;
  return p->exprs_len - 1;
}

// forward declare parse_addition to use in parse_primary
ExprIndex parse_addition(Parser *p);

// the most bottom level expression in the grammar
ExprIndex parse_primary(Parser *p) {
  if (p->lookahead.kind == TK_NUMBER) {
    Token number_token = parser_eat(p, TK_NUMBER);
    char *endptr = NULL;
    long double number = strtold(number_token.text, &endptr);
    assert(endptr == number_token.text + number_token.text_len);

    Expr number_expr = (Expr){.kind = EXPR_NUMBER, .data.number = number};
    return parser_push_expr(p, number_expr);
  }

  if (p->lookahead.kind == TK_OPEN_PAREN) {
    parser_eat(p, TK_OPEN_PAREN);
    ExprIndex expr_in_parens = parse_addition(p);
    parser_eat(p, TK_CLOSED_PAREN);

    return expr_in_parens;
  }

  if (p->lookahead.kind == TK_SUB) {
    parser_eat(p, TK_SUB);
    ExprIndex expr_after_minus = parse_primary(p);

    Expr neg_expr = (Expr){
        .kind = EXPR_NEG,
        .data.unary_op = expr_after_minus,
    };
    return parser_push_expr(p, neg_expr);
  }

  if (p->lookahead.kind == TK_IDENT) {
    Token ident = parser_eat(p, TK_IDENT);

    if (p->lookahead.kind == TK_OPEN_PAREN) {
      parser_eat(p, TK_OPEN_PAREN);
      ExprIndex argument = parse_addition(p);
      parser_eat(p, TK_CLOSED_PAREN);

      Expr func_call = (Expr){
          .kind = EXPR_FUNC,
          .data.func.argument = argument,
          .data.func.name = ident.text,
          .data.func.name_len = ident.text_len,
      };
      return parser_push_expr(p, func_call);
    } else {
      Expr var = (Expr){
          .kind = EXPR_VAR,
          .data.var.name = ident.text,
          .data.var.name_len = ident.text_len,
      };
      return parser_push_expr(p, var);
    }
  }

  assert(false && "Expecteed primary expression");
}

ExprIndex parse_exponent(Parser *p) {
  ExprIndex exponent_expr = parse_primary(p);
  while (1) {
    if (p->lookahead.kind == TK_POW) {
      parser_eat(p, TK_POW);
    } else {
      break;
    }

    ExprIndex right = parse_primary(p);
    Expr left = (Expr){
        .kind = EXPR_POW,
        .data.binary.left = exponent_expr,
        .data.binary.right = right,
    };
    exponent_expr = parser_push_expr(p, left);
  }

  return exponent_expr;
}

ExprIndex parse_implicit_mul(Parser *p) {
  ExprIndex implicit_mul_expr = parse_exponent(p);
  while (1) {
    if (p->lookahead.kind != TK_NUMBER && p->lookahead.kind != TK_IDENT &&
        p->lookahead.kind != TK_OPEN_PAREN) {
      break;
    }

    ExprIndex right = parse_exponent(p);
    Expr left = (Expr){
        .kind = EXPR_MUL,
        .data.binary.left = implicit_mul_expr,
        .data.binary.right = right,
    };
    implicit_mul_expr = parser_push_expr(p, left);
  }

  return implicit_mul_expr;
}

ExprIndex parse_multiplication(Parser *p) {
  ExprIndex multiplication_expr = parse_implicit_mul(p);
  while (1) {
    ExprKind kind;
    if (p->lookahead.kind == TK_MUL) {
      parser_eat(p, TK_MUL);
      kind = EXPR_MUL;
    } else if (p->lookahead.kind == TK_DIV) {
      parser_eat(p, TK_DIV);
      kind = EXPR_DIV;
    } else {
      break;
    }

    ExprIndex right = parse_implicit_mul(p);
    Expr left = (Expr){
        .kind = kind,
        .data.binary.left = multiplication_expr,
        .data.binary.right = right,
    };

    multiplication_expr = parser_push_expr(p, left);
  }

  return multiplication_expr;
}

// the most top level expression in the grammar
ExprIndex parse_addition(Parser *p) {
  ExprIndex addition_expr = parse_multiplication(p);
  while (1) {
    ExprKind kind;
    if (p->lookahead.kind == TK_ADD) {
      parser_eat(p, TK_ADD);
      kind = EXPR_ADD;
    } else if (p->lookahead.kind == TK_SUB) {
      parser_eat(p, TK_SUB);
      kind = EXPR_SUB;
    } else {
      break;
    }

    ExprIndex right = parse_multiplication(p);
    Expr left = (Expr){
        .kind = kind,
        .data.binary.left = addition_expr,
        .data.binary.right = right,
    };

    addition_expr = parser_push_expr(p, left);
  }

  return addition_expr;
}

// == Evaluation ==

// Compare string literal to sized string
#define STR_EQ(s1, s2, s2len) \
  sizeof(s1) - 1 == s2len && strncmp(s1, s2, s2len) == 0

long double lookup_variable_by_name(char *name, size_t name_len) {
  if (STR_EQ("pi", name, name_len)) {
    return 3.141592653589793238462643383279502884L;
  } else if (STR_EQ("tau", name, name_len)) {
    return 6.28318530717958647692528676655900576839L ;
  } else if (STR_EQ("e", name, name_len)) {
    return 2.718281828459045235360287471352662498L;
  } else if (STR_EQ("phi", name, name_len)) {
    return 1.61803398874989484820458683436563811772L;
  } else if (STR_EQ("nan", name, name_len)) {
    return NAN;
  } else if (STR_EQ("inf", name, name_len)) {
    return INFINITY;
  } else {
    fprintf(stderr, "Unknown variable: %.*s\n", (int)name_len, name);
    assert(false);
  }
}

long double call_func_by_name(char *name, size_t name_len, long double arg) {
  if (strncmp("sqrt", name, name_len) == 0) {
    return sqrtl(arg);
  } else if (STR_EQ("cbrt", name, name_len)) {
    return cbrtl(arg);
  } else if (STR_EQ("sin", name, name_len)) {
    return sinl(arg);
  } else if (STR_EQ("cos", name, name_len)) {
    return cosl(arg);
  } else if (strncmp("tan", name, name_len) == 0) {
    return tanl(arg);
  } else if (STR_EQ("cot", name, name_len)) {
    return 1 / tanl(arg);
  } else if (STR_EQ("asin", name, name_len)) {
    return asinl(arg);
  } else if (STR_EQ("acos", name, name_len)) {
    return acosl(arg);
  } else if (STR_EQ("atan", name, name_len)) {
    return atanl(arg);
  } else if (STR_EQ("acot", name, name_len)) {
    return atanl(1 / arg);
  } else if (STR_EQ("ln", name, name_len)) {
    return logl(arg);
  } else if (STR_EQ("log10", name, name_len)) {
    return log10l(arg);
  } else if (STR_EQ("log2", name, name_len)) {
    return log2l(arg);
  } else if (STR_EQ("exp", name, name_len)) {
    return expl(arg);
  } else if (STR_EQ("tgamma", name, name_len)) {
    return tgammal(arg);
  } else if (STR_EQ("fact", name, name_len)) {
    return tgammal(arg + 1);
  } else {
    fprintf(stderr, "Unknown function: %.*s\n", (int)name_len, name);
    assert(false);
  }
}

long double eval_ast(Expr exprs[], ExprIndex current) {
  Expr *expr = &exprs[current];
  long double left, right, argument;
  switch (expr->kind) {
  case EXPR_NUMBER:
    return expr->data.number;
  case EXPR_ADD:
    left = eval_ast(exprs, expr->data.binary.left);
    right = eval_ast(exprs, expr->data.binary.right);
    return left + right;
  case EXPR_SUB:
    left = eval_ast(exprs, expr->data.binary.left);
    right = eval_ast(exprs, expr->data.binary.right);
    return left - right;
  case EXPR_MUL:
    left = eval_ast(exprs, expr->data.binary.left);
    right = eval_ast(exprs, expr->data.binary.right);
    return left * right;
  case EXPR_DIV:
    left = eval_ast(exprs, expr->data.binary.left);
    right = eval_ast(exprs, expr->data.binary.right);
    return left / right;
  case EXPR_POW:
    left = eval_ast(exprs, expr->data.binary.left);
    right = eval_ast(exprs, expr->data.binary.right);
    return pow(left, right);
  case EXPR_VAR:
    return lookup_variable_by_name(expr->data.var.name,
                                   expr->data.var.name_len);
  case EXPR_FUNC:
    argument = eval_ast(exprs, expr->data.func.argument);
    return call_func_by_name(expr->data.func.name, expr->data.func.name_len,
                             argument);
  case EXPR_NEG:
    argument = eval_ast(exprs, expr->data.unary_op);
    return -argument;

  }
}

int main(void) {
  char *input = NULL;
  size_t input_len = 0;
  size_t nread;

  bool is_tty = isatty(fileno(stdin));

  static const size_t INITIAL_CAP = 512;
  Parser parser = (Parser){
      .exprs = malloc(sizeof(Expr) * INITIAL_CAP),
      .exprs_cap = INITIAL_CAP,
  };

  while (true) {
    if (is_tty) {
      printf(" -> ");
      fflush(stdout);
    }

    nread = getline(&input, &input_len, stdin);
    if ((long)nread <= 0) {
      break;
    }

    Lexer lexer = (Lexer){.cursor = input};
    Token first_token = lexer_next_nonspace(&lexer);
    if (first_token.kind == TK_NULL) {
      continue;
    }

    parser.lexer = lexer;
    parser.lookahead = first_token;
    parser.exprs_len = 0; // forget about all the exprs in previous iterations

    ExprIndex expr = parse_addition(&parser);

    // no more tokens should be left after parsing the expression
    assert(parser.lookahead.kind == TK_NULL);

    long double answer = eval_ast(parser.exprs, expr);
    if (fabsl(answer) <= 0.00000000000001) { // otherwise sin(pi) is ugly
      printf("0\n");
    } else {
      printf("%Lg\n", answer);
    }
  }

  free(input);
  free(parser.exprs);
  return 0;
}
