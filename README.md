# Programming-languages

1) The types defined in our language: SOL, SET, ENV , VAL and other functions.
SOL - a language that we defined as, among other things, 3 operations: cutting, merging and multiplying by scalar. We also defined global and static and a Number variable.
SET - A list of numbers.
ENV - This is a new environment we defined . Within it we defined VAL and different types of builders .
VAL - two constructors funV and SetV 

2) I used this code snippet in call-static because we do not want to change the values but only know what their values are.
with take one symbol and funv take two symbol, so we send same symbol twice.

3) in this procedure we dont use tail-recursion.
the advantage of tail-recursion is that thre is no need to wait to the stop conditions  to start calculating.

4) In the first, second functions we preferred to use call-static because we only want to know the values and not change them.
In the cons function we preferred to use call-dynamic because we wanted to allow the list to change if necessary.

5)The test will pass, even though in practice it is not supposed to work.
