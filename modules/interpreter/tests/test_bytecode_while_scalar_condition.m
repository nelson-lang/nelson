%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
% <--REUSE PROCESS-->
%=============================================================================
s = 0;
k = 1;
while k <= 5
  s = s + k;
  k = k + 1;
end
asserts.isequal(s, 15);
asserts.isequal(k, 6);
%=============================================================================
s = 0;
flag = true;
while flag
  s = s + 1;
  flag = false;
end
asserts.isequal(s, 1);
%=============================================================================
s = 0;
flag = 1;
while flag
  s = s + 1;
  flag = 0;
end
asserts.isequal(s, 1);
%=============================================================================
s = 0;
flag = uint8(1);
while flag
  s = s + 1;
  flag = uint8(0);
end
asserts.isequal(s, 1);
%=============================================================================
s = 0;
flag = single(1);
while flag
  s = s + 1;
  flag = single(0);
end
asserts.isequal(s, 1);
%=============================================================================
s = 0;
flag = 'a';
while flag
  s = s + 1;
  flag = char(0);
end
asserts.isequal(s, 1);
%=============================================================================
s = 0;
while false
  s = s + 1;
end
asserts.isequal(s, 0);
%=============================================================================
n = 160001;
p = 2;
hits = 0;
while p <= 32
  if mod(n, p) == 0
    hits = hits + 1;
  end
  p = p + 1;
end
asserts.isequal(hits, 0);
asserts.isequal(p, 33);
%=============================================================================
n = 24;
p = 2;
factors = [];
while n ~= 1
  while mod(n, p) == 0
    factors = [factors, p];
    n = n / p;
  end
  p = p + 1;
end
asserts.isequal(factors, [2 2 2 3]);
asserts.isequal(n, 1);
%=============================================================================
n = 25;
p = 2;
misses = 0;
while mod(n, p) ~= 0
  misses = misses + 1;
  p = p + 1;
end
asserts.isequal(misses, 3);
asserts.isequal(p, 5);
%=============================================================================
x = 1:8;
k = 1;
s = 0;
while k <= 8
  current = x(k);
  s = s + min(max(current, 2), 6);
  x(k) = current + 1;
  k = k + 1;
end
asserts.isequal(s, 34);
asserts.isequal(x, 2:9);
%=============================================================================
mod = zeros(1, 3);
k = 2;
hits = 0;
while k <= 3
  if mod(k) == 0
    hits = hits + 1;
  end
  k = k + 1;
end
asserts.isequal(hits, 2);
clear mod;
%=============================================================================
