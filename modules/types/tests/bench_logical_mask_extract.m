%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--RELEASE ONLY-->
%=============================================================================
% Bench: logical-mask extraction stays a single typed gather pass instead of
% materializing an ordinal index vector plus a per-element copy loop.
x = sin(1:1e6);
mask = x > 0;
%=============================================================================
R = 20;
tic();
for r = 1:R
  y = x(mask);
end
t = toc();
asserts.isequal(numel(y), sum(mask));
fprintf('  logical extract 1e6:  %0.2f ms/call\n', t / R * 1e3);
asserts.istrue(t / R < 20e-3);
%=============================================================================
tic();
for r = 1:R
  y2 = x(x > 0);
end
t = toc();
asserts.isequal(numel(y2), numel(y));
fprintf('  compare+extract 1e6:  %0.2f ms/call\n', t / R * 1e3);
%=============================================================================
% behavior checks: shapes and bounds mirror the ordinal-index rules
v = 10:10:50;
asserts.isequal(v(logical([1 0 1 0 1])), [10 30 50]);
c = (10:10:50)';
asserts.isequal(c(logical([1 0 1 0 1])), [10; 30; 50]);
m = magic(4);
asserts.isequal(m(m > 12), [16; 14; 15; 13]);
asserts.isequal(m(logical(eye(4))), [16; 11; 6; 1]);
i8 = int8(1:5);
asserts.isequal(i8(logical([0 1 1 0 0])), int8([2 3]));
ch = 'abcde';
asserts.isequal(ch(logical([1 0 0 0 1])), 'ae');
z = complex(1:4, 4:-1:1);
asserts.isequal(z(logical([0 1 0 1])), z([2 4]));
asserts.isequal(v(false(1, 5)), zeros(1, 0));
ok = false;
try
  q = v(logical([0 0 0 0 0 1]));
catch
  ok = true;
end
asserts.istrue(ok);
%=============================================================================
