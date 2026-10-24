%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
s = tf('s');
sys = (s + 3) / (s ^ 2 + 3 * s + 2);
tic();
for k = 1:5
  sysd = c2d(sys, 0.1);
  sysc = d2c(sysd);
  p = pole(sysc);
end
toc();
asserts.isequal(class(sysd), 'tf');
asserts.isequal(sysd.Variable, 'z');
asserts.isequal(class(sysc), 'tf');
asserts.isequal(sysc.Variable, 's');
asserts.isequal(size(p, 1), 2);
%=============================================================================
