%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
f = [8; 1; 3; 4; 2];
intcon = [2 4 5];
A = [-1 -2 0 0 0; -4 -1 -1 0 0; 2 1 1 1 1; 0 0 -1 -2 -1];
b = [14; -33; 30; -3];
lb = zeros(5, 1);
ub = [Inf; Inf; Inf; 8; 8];
opts = optimoptions('intlinprog', 'Display', 'off');
for k = 1:20
  [x, fval, exitflag] = intlinprog(f, intcon, A, b, [], [], lb, ub, [], opts);
end
asserts.istrue(exitflag > 0);
asserts.istrue(isfinite(fval));
%=============================================================================
