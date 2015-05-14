-- Test
true = \x y. x;
false = \x y. y;

if = \p x y. p x y;

test = 
    { x = 12
    , y = true
    , z = "two"
    };

zero = \f x. x;
one = succ zero;
two = succ one;

-- main = if true 1 2; 
plus = \x y. x + y;
testAdd = { p = plus 1 };
main = testAdd.p 1;
