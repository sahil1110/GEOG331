timeplot
print('profile_r3_1.png', '-dpng');

f= @(x,r) r.*x.*(1-x);
r= 3.1;
g= @(x) f(x,r);
cobweb(g, 1./3, 50);
print('cobweb_logistic1.png', '-dpng');

r= 2.9;
h= @(x) f(x,r);
cobweb(h, 1./3, 20);
print('cobweb_logistic2.png', '-dpng');

%f= @(x) 1.5.*x.*(2-x);
%cobweb(f, 3./5, 50, [0 2 0 2])
%print('test_cobweb.png', '-dpng'); 