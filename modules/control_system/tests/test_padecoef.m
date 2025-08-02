%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
T = 2; N = 4;
[numerator, denominator] = padecoef(T, N);
assert_isapprox(numerator, [1.0000  -10.0000   45.0000 -105.0000  105.0000], 1e-3);
assert_isapprox(denominator, [1.0000   10.0000   45.0000  105.0000  105.0000], 1e-3);
%=============================================================================
[numerator, denominator] = padecoef(T);
assert_isapprox(numerator, [-1 1], 1e-3);
assert_isapprox(denominator, [1 1], 1e-3);
%=============================================================================