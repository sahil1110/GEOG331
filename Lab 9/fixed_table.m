f= @(x) x.^2-3;
g1= @(x) (x.^3)/3;
g2= @(x) x- (x.^2-3);
g3= @(x) x- ((x.^2-3)/2);
x0= 1.5;
fid= fopen('fixed_table.txt', 'w');
fprintf(fid, 'n=%2i& x(g1)=%18.12e& x(g2)=%18.12e& x(g3)= %18.12e\\\\',0, 1.5, 1.5, 1.5); 
xprev1= g1(x0);
xprev2= g2(x0);
xprev3= g3(x0);

for i=1:20
    fprintf(fid,'n=%2i& x(g1)=%18.12e& x(g2)=%18.12e& x(g3)= %18.12e\\\\',i, xprev1, xprev2, xprev3);
    xprev1= g1(xprev1);
    xprev2= g2(xprev2);
    xprev3= g3(xprev3);
end

fclose(fid);