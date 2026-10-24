%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
A = [0 1; -2 -3];
B = [0; 1];
C = [1 0];
D = 0;
sys = ss(A, B, C, D);
tic();
for k = 1:100
  g = dcgain(sys);
  p = pole(sys);
  c = ctrb(A, B);
  o = obsv(A, C);
  r = rank(c) + rank(o);
end
toc();
asserts.istrue(isnumeric(g));
asserts.isequal(size(p, 1), 2);
asserts.isequal(r, 4);
%=============================================================================
