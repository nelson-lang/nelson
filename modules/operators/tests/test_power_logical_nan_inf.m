%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% NaN propagates through a zero exponent
asserts.isequal(NaN ^ 0, NaN);
asserts.isequal(NaN .^ 0, NaN);
asserts.isequal(power(NaN, 0), NaN);
asserts.isequal([NaN 1] .^ 0, [NaN 1]);
asserts.isequal(1 .^ NaN, NaN);
%=============================================================================
% negative base with an infinite exponent is real
asserts.isequal((-1) ^ Inf, 1);
asserts.isequal((-1) ^ -Inf, 1);
asserts.isequal((-1) .^ [Inf -Inf], [1 1]);
asserts.isequal((-2) ^ Inf, Inf);
asserts.isequal((-0.5) ^ Inf, 0);
%=============================================================================
% an odd integer exponent keeps the sign of an infinite or zero base
asserts.isequal((-Inf) .^ 3, -Inf);
asserts.isequal((-Inf) ^ 3, -Inf);
asserts.isequal((-Inf) .^ [1 2 3], [-Inf Inf -Inf]);
asserts.isequal([-Inf 1] .^ 3, [-Inf 1]);
asserts.isequal(1 ./ ((-Inf) .^ -3), -Inf);
asserts.isequal(1 ./ ((-Inf) .^ -2), Inf);
asserts.isequal((-0) .^ -1, -Inf);
asserts.isequal((-0) .^ -2, Inf);
asserts.isequal(1 ./ ((-0) .^ 3), -Inf);
asserts.isequal(1 ./ ((-0) .^ 2), Inf);
%=============================================================================
% logical operands of the arithmetic operators compute as double
asserts.isequal(true * true, 1);
asserts.isequal(class(true * true), 'double');
asserts.isequal(true / true, 1);
asserts.isequal(true / false, Inf);
asserts.isequal(false / false, NaN);
asserts.isequal(true ^ true, 1);
asserts.isequal(class(true ^ true), 'double');
asserts.isequal(true(2) * true(2), [2 2; 2 2]);
asserts.isequal(class(true(2) * true(2)), 'double');
%=============================================================================
