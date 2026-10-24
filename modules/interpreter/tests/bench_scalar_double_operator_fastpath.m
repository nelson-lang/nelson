%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--REUSE PROCESS-->
% <--WEIGHT 8-->
%=============================================================================
% <--RELEASE ONLY-->
%=============================================================================
N = 1000000;
x = 3;
y = 2;
fprintf('bench_scalar_double_operator_fastpath: N=%d\n', int32(N));
%=============================================================================
tic();
for i = 1:N
  z = -x;
end
t1 = toc();
asserts.isequal(z, -3);
fprintf('  unary minus: %0.4f s\n', t1);
%=============================================================================
tic();
for i = 1:N
  z = +x;
end
t2 = toc();
asserts.isequal(z, 3);
fprintf('  unary plus: %0.4f s\n', t2);
%=============================================================================
tic();
for i = 1:N
  z = x.';
end
t3 = toc();
asserts.isequal(z, 3);
fprintf('  dot transpose: %0.4f s\n', t3);
%=============================================================================
tic();
for i = 1:N
  z = x & y;
end
t4 = toc();
asserts.istrue(z);
fprintf('  logical and: %0.4f s\n', t4);
%=============================================================================
tic();
for i = 1:N
  z = x | 0;
end
t5 = toc();
asserts.istrue(z);
fprintf('  logical or: %0.4f s\n', t5);
%=============================================================================
tic();
for i = 1:N
  z = x \ y;
end
t6 = toc();
asserts.isequal(z, y / x);
fprintf('  matrix left divide: %0.4f s\n', t6);
%=============================================================================
tic();
for i = 1:N
  z = x .\ y;
end
t7 = toc();
asserts.isequal(z, y / x);
fprintf('  left divide: %0.4f s\n', t7);
%=============================================================================
tic();
for i = 1:N
  z = x';
end
t8 = toc();
asserts.isequal(z, x);
fprintf('  complex transpose: %0.4f s\n', t8);
%=============================================================================
tic();
for i = 1:N
  if x && true
    z = 1;
  end
end
t9 = toc();
asserts.isequal(z, 1);
fprintf('  shortcut and: %0.4f s\n', t9);
%=============================================================================
tic();
for i = 1:N
  if 0 || x
    z = 1;
  end
end
t10 = toc();
asserts.isequal(z, 1);
fprintf('  shortcut or: %0.4f s\n', t10);
%=============================================================================
