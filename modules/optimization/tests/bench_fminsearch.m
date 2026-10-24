%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
tic
opts = optimset('TolX', 1e-7, 'TolFun', 1e-7, 'MaxIter', 200, 'MaxFunEvals', 400);
target = [1; -2];
[x, fval] = fminsearch(@(x) sum((x(:) - target) .^ 2), zeros(2, 1), opts);
toc
asserts.istrue(max(abs(x(:) - target)) < 1e-3);
asserts.istrue(abs(fval) < 1e-6);
%=============================================================================
