%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
X = [1 2 4 8 16 32];
windowSize = 3;
b = (1 / windowSize) * ones(1, windowSize);
a = 1;
y = filter(b, a, X);
REF = [1 / 3 1 7 / 3 14 / 3 28 / 3 56 / 3];
asserts.isapprox(y, REF, 1e-12);
[filteredValues, finalState] = filter([1 1], 1, [1 2 3], 0);
asserts.isequal(filteredValues, [1 3 5]);
asserts.isequal(finalState, 3);
%=============================================================================
