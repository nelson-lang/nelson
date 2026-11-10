%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
xv = [0 1 1 0];
yv = [0 0 1 1];
[in, on] = inpolygon([0.5 2 1 0 0.5], [0.5 2 0 0.5 1], xv, yv);
assert_isequal(in, [true false true true true]);
assert_isequal(on, [false false true true true]);
[in, on] = inpolygon([0.5 3], [0.5 0.5], [0 1 1 0 NaN 2 4 4 2], [0 0 1 1 NaN 0 0 1 1]);
assert_isequal(in, [true true]);
assert_isequal(on, [false false]);
assert_isequal(size(inpolygon(ones(2, 3) / 2, ones(2, 3) / 2, xv, yv)), [2 3]);
assert_isequal(inpolygon([1 2.5 1.5], [1 1 2.5], [0 3 3 2 2 0], [0 0 3 3 1 1]), [true true false]);
outer = [0 4 4 0 NaN 1 3 3 1];
outerY = [0 0 4 4 NaN 1 1 3 3];
assert_isequal(inpolygon([0.5 2], [0.5 2], outer, outerY), [true false]);
assert_checkerror('inpolygon(1, [1 2], xv, yv)', _('X and Y must have the same size, and XV and YV the same number of elements.'));
%=============================================================================
