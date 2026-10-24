%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
expr = optimexpr(2, 1);
asserts.isequal(evaluate(expr, struct()), zeros(2, 1));
x = optimvar('x');
expr = optimexpr(x + 1);
asserts.isequal(evaluate(expr, struct('x', 2)), 3);
%=============================================================================
