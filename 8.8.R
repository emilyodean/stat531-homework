# part 1
x = seq(-3, 3, 0.01);
fX = x^3 + 2*x^2 - 7;
plot(x, fX, type="l");

# part 2
x =1;
x^3 + 2*x^2 - 7;

# part 3
x =1;
3*x^2 + 4*x;

# part 4
(x^3 + 2*x^2 - 7) / (3*x^2 + 4*x);

# part 5
x - (x^3 + 2*x^2 - 7) / (3*x^2 + 4*x)

# part 6
x = 1;
fX = x^3 + 2*x^2 - 7;
fXPrime = 3*x^2 + 4*x;
counter = 0;
precision = 0.000001;
while(abs(fX) > precision)
{
    x = x - fX / fXPrime;
    counter = counter + 1;
    fX = x^3 + 2*x^2 - 7;
    fXPrime = 3*x^2 + 4*x;
}
myRoot = x;
c(counter, myRoot);

x^3 + 2*x^2 - 7;

# part 7
x = seq(-3, 3, 0.01);
fX = x^3 + 2*x^2 - 7;
plot(x, fX, type="l");
abline(v=myRoot, col=2);
abline(h=0, lty=2);

# part 8
myFun = function(x)
{
    x^3 + 2*x^2 - 7;
}
myFunPrime = function(x)
{
    3*x^2 + 4*x;
}

# part 9
myFun(1);
myFunPrime(1);

# part 10
x = 1;
counter = 0;
precision = 0.000001;
while(abs(myFun(x)) > precision)
{
    x = x - myFun(x)/myFunPrime(x);
    counter = counter + 1;
}
myRoot = x;
c(counter, myRoot);

# part 11
x = 1;
counter = 0;
precision = 1e-10;
maxCount = 50;
while( (abs(myFun(x)) > precision) & (counter<maxCount))
{
    x = x - myFun(x)/myFunPrime(x);
    counter = counter + 1;
}
myRoot = x;
c(counter, myRoot);

# part 12
myEnv = new.env();
assign("x", 1, envir=myEnv);
myFunDer = numericDeriv(quote(myFun(x)), c("x"), myEnv);

# part 13
attr(myFunDer, "gradient");

# part 14
x = 1;
counter = 0;
precision = 1e-10;
maxCount = 50;
while( (abs(myFun(x)) > precision)&(counter<maxCount))
{
    myEnv = new.env();
    assign("x", x, envir=myEnv);
    myFunDer = numericDeriv(quote(myFun(x)), c("x"), myEnv);
    
    x = x - myFun(x)/attr(myFunDer, "gradient");
    counter = counter + 1;
}
myRoot = x;
c(counter, myRoot);