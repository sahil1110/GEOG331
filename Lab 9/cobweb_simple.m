function m = cobweb_simple(xvalues)
xs= zeros(1, 2*length(xvalues)-1);
ys= zeros(1, 2*length(xvalues)-1);
counter=1;

for i=1:length(xvalues)-1
    xs(1, counter)= xvalues(1, i);
    ys(1, counter)= xs(1, counter);
    xs(1, counter+1)= xs(1, counter);
    ys(1, counter+1)= xvalues(1, i+1);
    counter= counter+2;
end
xs(1, counter)= xvalues(1, length(xvalues));
ys(1, counter)= xs(1, counter);
m= [xs;ys]
end