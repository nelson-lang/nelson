%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
asserts.checkerror('imgaussfilt([1 2], 0)', sprintf(_('%s must contain positive finite values.'), 'Sigma'));
%=============================================================================
asserts.checkerror('imgaussfilt([1 2], 1 + i)', sprintf(_('%s must contain positive finite values.'), 'Sigma'));
%=============================================================================
asserts.checkerror('imgaussfilt([1 2], [1 2 3])', sprintf(_('Sigma must be a scalar or %d-element vector.'), 2));
%=============================================================================
asserts.checkerror('imgaussfilt([1 2], 1, ''FilterSize'', [2 3])', sprintf(_('%s size must contain positive odd integers.'), 'Filter'));
%=============================================================================
asserts.checkerror('imgaussfilt([1 2], 1, ''FilterSize'', [3 + i 3])', sprintf(_('%s size must contain positive odd integers.'), 'Filter'));
%=============================================================================
asserts.checkerror('imgaussfilt([1 2], ''Unknown'', true)', sprintf(_('Unsupported "%s" option.'), 'imgaussfilt'));
%=============================================================================
asserts.checkerror('imgaussfilt([1 2], 1, ''FilterSize'')', _('Name-value arguments must be paired.'));
%=============================================================================
asserts.checkerror('imgaussfilt([1 2], 1, 1, true)', sprintf(_('%s option name must be a string.'), 'imgaussfilt'));
%=============================================================================
asserts.checkerror('imgaussfilt([1 2], ''FilterDomain'', ''unknown'')', sprintf(_('Unsupported "%s" option.'), 'imgaussfilt'));
%=============================================================================
asserts.checkerror('imgaussfilt([1 2], ''FilterDomain'', 1)', sprintf(_('Unsupported "%s" option.'), 'imgaussfilt'));
%=============================================================================
asserts.checkerror('imgaussfilt([1 2], 1, ''Padding'', ''unknown'')', sprintf(_('Unsupported "%s" option.'), 'padding'));
%=============================================================================
asserts.checkerror('imgaussfilt([1 2], 1, ''Padding'', Inf)', sprintf(_('%s must be finite.'), 'Padding value'));
%=============================================================================
asserts.checkerror('imgaussfilt(sparse([1 2]))', sprintf(_('%s are not supported.'), 'Sparse images'));
%=============================================================================
asserts.checkerror('imgaussfilt(zeros(2, 2, 5))', sprintf(_('%s must be a 2-D, RGB, or RGBA array.'), 'Image'));
%=============================================================================
asserts.checkerror('imgaussfilt({1 2})', sprintf(_('%s must be numeric or logical.'), 'Image'));
