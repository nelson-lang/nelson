%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% a scalar window w spans [k - w/2, k + w/2) around element k, also for a
% noninteger w: floor(w/2) elements before, ceil(w/2) - 1 after
%=============================================================================
x = [5 1 4 2 8 3 7];
assert_isequal(movsum(x, 2.5), [6 10 7 14 13 18 10]);
assert_isequal(movmax(x, 4.5), [5 5 8 8 8 8 8]);
assert_isequal(movmin(x, 3.7), [1 1 1 2 2 3 3]);
assert_isequal(movsum(x, 1.5), x);
assert_isequal(movsum(x, 6.5), [12 20 23 30 25 24 20]);
assert_isequal(movsum(x, [1.5 0.5]), [5 6 5 6 10 11 10]);
assert_isequal(movsum(x, 2.5, 'Endpoints', 'discard'), [10 7 14 13 18]);
assert_isequal(movmean(1:6, 2.5), [1.5 2 3 4 5 5.5]);
assert_isequal(movmean([1 NaN 3 NaN 5], 2.5, 'omitnan'), [1 2 3 4 5]);
assert_isequal(movmedian(1:6, 3.2), [1.5 2 3 4 5 5.5]);
%=============================================================================
% integer windows are unchanged
assert_isequal(movsum(x, 2), [5 6 5 6 10 11 10]);
assert_isequal(movsum(x, 3), [6 10 7 14 13 18 10]);
assert_isequal(movsum(x, 4), [6 10 12 15 17 20 18]);
%=============================================================================
