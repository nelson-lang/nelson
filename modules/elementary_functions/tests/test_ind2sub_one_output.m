%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% single output: the linear index is returned unchanged (as the expected behavior does),
% even when it exceeds prod(dims)
R = ind2sub([2 3], 7);
asserts.isequal(R, 7);
R = ind2sub([2 3], 5);
asserts.isequal(R, 5);
R = ind2sub([2 3], [1 2 3]);
asserts.isequal(R, [1 2 3]);
% multiple outputs unchanged
[r, c] = ind2sub([2 3], 5);
asserts.isequal(r, 1);
asserts.isequal(c, 3);
[i1, i2, i3] = ind2sub([2 3 4], 17);
asserts.isequal([i1 i2 i3], [1 3 3]);
%=============================================================================
