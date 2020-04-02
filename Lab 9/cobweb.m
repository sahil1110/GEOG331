function [] = cobweb(f, x0, N, rect)

if nargin<4 || isempty(rect)
    rect= [0 1 0 1];
end

xvalues= zeros(1, N+1);
xvalues(1)= x0;
xprev= x0;

for i=2:N+1
    x_n1= f(xprev);
    xvalues(i)= x_n1;
    xprev= x_n1;
end

coords= cobweb_simple(xvalues);
xs= coords(1,:);
ys= coords(2,:);

plot([0 1], [0 1], '--k')
hold on
xfunc= linspace(rect(1), rect(2), 200);
yfunc= f(xfunc);
plot(xfunc, yfunc, '-k')
plot(xs, ys, '-bo')
title(sprintf('Solution Profile for f(x)= %s',func2str(f)));
xlabel('n'); ylabel('x')
hold off
    
end