%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
n = 20;
f = -(1:n)';
A = [eye(n); -eye(n); ones(1, n)];
b = [ones(n, 1); zeros(n, 1); 8];
opts = optimoptions('linprog', 'Display', 'off');
for k = 1:50
  [x, fval, exitflag] = linprog(f, A, b, [], [], [], [], opts);
end
asserts.istrue(exitflag > 0);
asserts.istrue(isfinite(fval));
%=============================================================================
