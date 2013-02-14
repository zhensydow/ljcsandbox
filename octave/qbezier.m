function [x, y] = qbezier(p0,p1,p2)
  x = 0:100;
  y = 0:100;
  for i = 0:100
    t = i / 100;
    s = 1 - t;
    
    res = p0*(s*s) + (p1*(2*s*t)) + (p2*(t*t));
    x(i+1) = res(1);
    y(i+1) = res(2);
  endfor
endfunction
