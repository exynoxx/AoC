"""
///simple
1+2;(1+2)*3/2;5//2;5**6;
true & false; true | false; true ^ false; !true & !false;

///arrays
a=[1,2,3];a=I3+1;
b=[5]*10 /// 5 5 5 5....
size=a?
sum = +/a
avg = +/a/<.?
avg2 = a.+/a.?
avg3 = a.+/a?
avg4 = a.+/?/<
zero = a$0
2d = [c=[1,2],[3,4]];
c.+ /// =3
three = 2d$1$0;
P+/I10+1; ///print 55
D$$ /// =I ///sort
D.$$ /// =I
$${-x}/D ///=D
[1,1,1]+5 == [5,5,5] ///true
[1,1,1]+=5 ///[1,1,1,5]


///functions
{x}; /// x=>x
{x+y}; /// (x,y) => x+yM
{P x}; ///x is now input to {} which is now an anon func
{x;y;z;P x;P y+z;}; ///define order of anon variables first

///anon args
{a1+a3+a2} /// (x,y,z)=>x+z+y

map = {f;l;l.f}
map = {a2.a1}
add5 = {x+5}
one = [1]*10;
six = map add5 one;
six = map{x+5}[1]*10;


///control flow
true : P"T" : P"F"
!false : P"only if";

///inf loop
{...}*_;

///do 5 times
{P}*5;5*{P}
[1,2,3]/P;P[1,2,3];PI3+1


///strings
"hello" + " " + "world";
P"H"+"e"*10 + "llo"; print("Heeeeeeeeeello")
"abc"/


///string-int conversion
#/5; 5.#; ///"5"
"5".# + "5".# /// = 10

///set
ten = {I10}


+-
*/
& | ! ^

"""

"""
expression ::= number (("|" | "&" | "^" | "!") number)*
number ::= term ((+|-) term)*
term ::= factor ((*|/) factor)*
factor ::= _ | int | true | false | ( expression ) | block | a.x
"""
from compynator import Forward, Terminal, Digit, Succeed, Alpha

expr = Forward()
factor = Terminal("true") | Terminal("false") | Digit.repeat(1).value(int) | Alpha.value(10)

term_trail = Forward()
term_trail.is_(
    (Terminal("*")|Terminal("/")).value(1).then(factor).then(term_trail, lambda x, y: x * y) | Succeed(False).value(1)
)
term = factor.then(term_trail, lambda x,y:x*y)

number_trail = Forward()
number_trail.is_(
    (Terminal("+")|Terminal("-")).value(1).then(term).then(number_trail, lambda x, y: x + y) | Succeed(False).value(1)
)
number = term.then(number_trail, lambda x,y:x*y)


# number = term.then((Terminal("+") | Terminal("-")).then(term)).repeat()
# expr.is_(number.then((Terminal("&") | Terminal("|")).then(number)).repeat())
# expr.is_(term)
print(expr.parse("a*a*a"))




"""
# expr = Forward()
factor = Terminal("true") | Terminal("false") | Digit.repeat(1).value(int) | Alpha.value(10)

expr_trail = Forward()
expr_trail.is_(
    (Terminal("*")|Terminal("/")).value(1).then(factor).then(expr_trail, lambda x, y: x * y) | Succeed(False).value(1)
)
expr = factor.then(expr_trail, lambda x,y:x*y)

# number = term.then((Terminal("+") | Terminal("-")).then(term)).repeat()
# expr.is_(number.then((Terminal("&") | Terminal("|")).then(number)).repeat())
# expr.is_(term)
print(expr.parse("a*a*a"))"""