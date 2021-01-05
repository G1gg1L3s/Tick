# Grammar

## Expression precedence
The precedence of Tick operators and expressions is ordered as follows, going from strong to weak. Binary Operators at the same precedence level are grouped in the order given by their associativity.
This is mainly stolen from the Rust.

| Operator/Expression          | Associativity |
|------------------------------|---------------|
| Field Expressions            | left to right |
| fn calls, array indexing     |               |
| Unary `-` `@` `!` `&` `&mut` |               |
| `as`                         | left to right |
| `*` `/` `%`                  | left to right |
| `+` `-`                      | left to right |
| `==` `!=` `<` `>` `<=` `>=`  | ??            |
| `&&`                         | left to right |
| `||`                         | left to right |
| `=`                          | right to left |
| return break                 |               |

## Expression syntax
The following grammar is a draft. It's not the final and can have ambiguity.
But for now, I'm going to set minimalistic goals:
- Basic expressions (arithmetic, function calls, struct fields).
- Every block can be used as expressions with the same rules as in Rust.
- Everything is in global namespace (for simplicitly).
- `&` is address-of operator and `@` for deferencing.
- Infinite and while loops.

This syntax is mostly stolen from Rust but with simplifications.

```
Expression:
    ExpressionWithoutBlock
    | ExpressionWithBlock

StructlessExpression:
    StructlessExpressionWithoutBlock
    | ExpressionWithBlock

ExpressionWithoutBlock:
    StructlessExpressionWithoutBlock
    | StructExpression


StructlessExpressionWithoutBlock:
    LiteralExpression
    | OperatorExpression
    | GroupedExpression
    | ArrayExpression
    | IndexExpression
    | CallExpression
    | FieldExpression
    | ContinueExpression
    | BreakExpression
    | ReturnExpression

ExpressionWithBlock:
    BlockExpression
    | LoopExpression
    | IfExpression

BlockExpression:
    { Statements? }

Statements:
    Statement+ ExpressionWithoutBlock
    | ExpressionWithoutBlock

Statement:
    ;
   | Item
   | LetStatement
   | ExpressionStatement

LetStatement:
    let mut? IDENTIFIER ( : Type )? (= Expression )? ;

ExpressionStatement:
    ExpressionWithoutBlock ;
   | ExpressionWithBlock ;?

OperatorExpression:
    BorrowExpression
    | DereferenceExpression
    | NegationExpression
    | ArithmeticExpression
    | ComparisonExpression
    | LazyBooleanExpression
    | TypeCastExpression
    | AssignmentExpression

BorrowExpression:
    & Expression
    | & mut Expression

DereferenceExpression:
    @ Expression

NegationExpression:
    - Expression
   | ! Expression

ArithmeticExpression:
    Expression + Expression
   | Expression - Expression
   | Expression * Expression
   | Expression / Expression
   | Expression % Expression

ComparisonExpression:
    Expression == Expression
   | Expression != Expression
   | Expression > Expression
   | Expression < Expression
   | Expression >= Expression
   | Expression <= Expression

LazyBooleanExpression :
    Expression || Expression
   | Expression && Expression

TypeCastExpression:
   Expression as Type

AssignmentExpression:
   Expression = Expression

GroupedExpression:
   (Expression)

ArrayExpression:
   [ArrayElements?]

ArrayElements:
    Expression ( , Expression )*,?
   | Expression ; Expression

IndexExpression:
   Expression [ Expression ]

StructExpression:
    Ident { StructExprFields? }

StructExprFields:
   StructExprField (, StructExprField)* ,?

StructExprField:
    IDENTIFIER
   | IDENTIFIER : Expression

CallExpression:
   Expression ( CallParams? )

CallParams:
   Expression ( , Expression )* ,?


FieldExpression:
   Expression . IDENTIFIER

LoopExpression:
    InfiniteLoopExpression
    | PredicateLoopExpression

InfiniteLoopExpression:
   loop BlockExpression

PredicateLoopExpression:
   while StructlessExpression BlockExpression

ContinueExpression:
   continue

BreakExpression:
    break

IfExpression:
   if StructlessExpression BlockExpression
   (else ( BlockExpression | IfExpression ))?

ReturnExpression:
   return Expression?

```

## Items
Item in Tick has the approximately same meaning as in Rust.
Syntax is as follows:
```
Item:
    | Import
    | Function
    | TypeAlias
    | Struct
    | Enumeration
    | ConstantItem
    | StaticItem

Import:
    import StringLiteral ;

Function:
    fn IDENTIFIER ( FunctionParameters? ) FunctionReturnType? BlockExpression

FunctionParameters:
    FunctionParam (, FunctionParam)* ,?

FunctionParam:
    IDENTIFIER : Type

FunctionReturnType:
   -> Type

TypeAlias:
   type IDENTIFIER = Type ;

Struct:
    struct IDENTIFIER { StructFields? }

StructFields:
   StructField (, StructField)* ,?

StructField:
   IDENTIFIER : Type

enum IDENTIFIER { EnumItems? }

EnumItems:
   EnumItem (, EnumItem )* ,?

EnumItem:
   IDENTIFIER

ConstantItem:
   const IDENTIFIER : Type = Expression ;

StaticItem:
   static mut? IDENTIFIER : Type = Expression ;


Type:
    IDENTIFIER
    | PointerType
    | NeverType
    | ArrayType
    | VoidType

PointerType:
    & mut? Type

NeverType:
    !

ArrayType:
    [Type ; Expression]

VoidType:
    ()

```

## Example
With all of the rules, Tick language should look approximately as follows:
```
import "math"; // The same as #include <math.h> in c

struct S {
    a: i32,
}

const PI_2: f64 = PI * 2.0;

fn add_one(x: &mut i32) {
    @x + 1
}

fn main() -> i32 {
    let mut s = S{ a: -1 };
    add_one(&mut s.a);
    s.a
}

```