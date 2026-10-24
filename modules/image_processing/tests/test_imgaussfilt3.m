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
asserts.checkerror('imgaussfilt3([1 2], 1)', sprintf(_('%s must be a 3-D array.'), 'Volume'));
%=============================================================================
asserts.isequal(imgaussfilt3(cat(3, [1 2], [3 + i 4]), 1, 'FilterSize', [1 1 1]), ...
  cat(3, [1 2], [3 + i 4]));
%=============================================================================
asserts.checkerror('imgaussfilt3(sparse([1 2]))', sprintf(_('%s are not supported.'), 'Sparse volumes'));
%=============================================================================
asserts.checkerror('imgaussfilt3(ones(2, 2, 2), [1 2])', sprintf(_('Sigma must be a scalar or %d-element vector.'), 3));
%=============================================================================
asserts.checkerror('imgaussfilt3(ones(2, 2, 2), 0)', sprintf(_('%s must contain positive finite values.'), 'Sigma'));
%=============================================================================
asserts.checkerror('imgaussfilt3(ones(2, 2, 2), 1, ''FilterSize'', [3 3 4])', sprintf(_('%s size must contain positive odd integers.'), 'Filter'));
%=============================================================================
asserts.checkerror('imgaussfilt3(ones(2, 2, 2), ''Padding'', ''unknown'')', sprintf(_('Unsupported "%s" option.'), 'padding'));
%=============================================================================
asserts.checkerror('imgaussfilt3(ones(2, 2, 2), ''Padding'', Inf)', sprintf(_('%s must be finite.'), 'Padding value'));
%=============================================================================
asserts.checkerror('imgaussfilt3(ones(2, 2, 2), ''FilterDomain'', ''unknown'')', sprintf(_('Unsupported "%s" option.'), 'imgaussfilt3'));
%=============================================================================
asserts.checkerror('imgaussfilt3(ones(2, 2, 2), ''Unknown'', true)', sprintf(_('Unsupported "%s" option.'), 'imgaussfilt3'));
