%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 4-->
% deterministic function returns the expected value
sq = memoize(@(x) x .^ 2);
asserts.isequal(sq(5), 25);
asserts.isequal(sq(5), 25);
%=============================================================================
% multiple outputs are cached and returned
mo = memoize(@(x) deal(min(x), max(x)));
[lo1, hi1] = mo([3 1 2]);
[lo2, hi2] = mo([3 1 2]);
asserts.isequal([lo1, hi1], [1, 3]);
asserts.isequal([lo2, hi2], [1, 3]);
%=============================================================================
asserts.checkerror('memoize(3)', sprintf(_('%s must be a function handle.'), 'Input'), 'Nelson:memoize:invalidInput');
%=============================================================================
