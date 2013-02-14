function [x, y] = cbezier(p0,p1,p2,p3)
  x = 0:100;
  y = 0:100;
  for i = 0:100
    t = i / 100;
    s = 1 - t;
    
    res = p0*(s*s*s) + (p1*(3*s*s*t)) + (p2*(3*s*t*t)) + (p3*(t*t*t));
    x(i+1) = res(1);
    y(i+1) = res(2);
  endfor
endfunction
