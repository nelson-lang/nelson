%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
BW = false(32, 32);
BW(4:10, 4:10) = true;
BW(18:26, 16:24) = true;
CC = bwconncomp(BW);
L = labelmatrix(CC);
[L2, n] = bwlabel(BW);
asserts.isequal(CC.NumObjects, 2);
asserts.isequal(cellfun('length', CC.PixelIdxList), [49 81]);
asserts.isequal(L, L2);
asserts.isequal(n, 2);
%=============================================================================
asserts.checkerror('bwconncomp(sparse([1 0]))', sprintf(_('%s are not supported.'), 'Sparse images'));
%=============================================================================
asserts.checkerror('bwconncomp(logical([1 0]), 6)', _('Unsupported connectivity.'));
%=============================================================================
asserts.checkerror('bwconncomp(true(2, 2, 2), 4)', _('Unsupported connectivity.'));
