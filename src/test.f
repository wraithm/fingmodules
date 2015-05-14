-- Test
id = \A, x:A. x;
true = \A. \x:A, y:A. x;
false = \A. \x:A, y:A. y;

if = \A. \p:A->A->A, x:A, y:A. p x y;

test = 
    { x = 12
    , y = true
    , z = "two"
    };

-- test3 = unpack<t,x> = intCounter in x.tick x.c;

test4 = \x:Int, y:* -> *, z:{w:Int}. x;

zero = \A. \f:(A -> A), x:A. x;
succ = 
    \n:forall A. (A -> A) -> A -> A. 
    \A, f:(A -> A), x:A.
        f (n [A] f x);

one = succ zero;
two = succ one;

cnat2int =
    \m:forall A. (A -> A) -> A -> A. 
        m [Int] (\x:Int. x + 1) 0;

cnatCounter = 
    pack<forall a. (a -> a) -> a -> a,
        { c = zero
        , tick = succ
        , show = cnat2int }>
    as exists counter.
        { c : counter
        , tick : counter -> counter
        , show : counter -> Int };

plus = \x:Int, y:Int. x + y;

intCounter = 
    pack<Int,
        { c = 5
        , tick = plus 1
        , show = id [Int] }> 
    as exists counter. 
        { c : counter
        , tick : counter -> counter
        , show : counter -> Int };

tickCounter = 
    \c:exists counter. 
        {c : counter, tick : counter -> counter, show : counter -> Int }.
    unpack<Counter, counter> = c in
        counter.show (counter.tick counter.c);

main = 
    { church = tickCounter cnatCounter
    , int = tickCounter intCounter };

{-
main =
    unpack<Counter, counter> = intCounter in
    counter.tick counter.c;
-}

-- int2 = \x:*. x;

-- Parsing error...
-- test4 3 [int2] should be (test4 3) [int2] instead of test4 (3 [int2])
-- main = (test4 3) [int2] {w = 4};

-- main = if [Int] (true [Int]) 5 6;
-- main = cnat2int two;
