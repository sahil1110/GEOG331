f= @(x,r) r*x*(1-x);
N= 100;
tvalues= 0:N;
xvalues= zeros(1, N+1);
x0= 1/3;
xvalues(1)= x0;
r= 3.1;
xprev= x0;

for i=2:N+1
    x_n1= f(xprev, r);
    xvalues(i)= x_n1;
    xprev= x_n1;
end

plot(tvalues, xvalues, '-o');
title(sprintf('Solution Profile for f(x)= %s',func2str(f)));
xlabel('Time(t)'); ylabel('x')