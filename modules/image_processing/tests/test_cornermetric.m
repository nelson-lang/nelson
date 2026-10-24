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
asserts.checkerror('cornermetric({1})', sprintf(_('%s must be numeric or logical.'), 'Image'));
%=============================================================================
asserts.checkerror('cornermetric(sparse([1 0]))', sprintf(_('%s are not supported.'), 'Sparse images'));
%=============================================================================
asserts.checkerror('cornermetric([1 + i])', sprintf(_('%s must be real.'), 'Image'));
%=============================================================================
asserts.checkerror('cornermetric(zeros(2, 2, 2))', sprintf(_('%s must be a 2-D or RGB array.'), 'Image'));
%=============================================================================
asserts.checkerror('cornermetric([NaN])', sprintf(_('%s values must be finite.'), 'Image'));
%=============================================================================
asserts.checkerror('cornermetric([1 2], ''unknown'')', sprintf(_('Unsupported "%s" option.'), 'corner metric method'));
%=============================================================================
asserts.checkerror('cornermetric([1 2], ''FilterSize'')', sprintf(_('Unsupported "%s" option.'), 'corner metric method'));
%=============================================================================
asserts.checkerror('cornermetric([1 2], ''FilterSize'', 4)', ...
  _('FilterSize must be an odd integer scalar greater than or equal to 3.'));
%=============================================================================
asserts.checkerror('cornermetric([1 2], ''Unknown'', true)', sprintf(_('Unsupported "%s" option.'), 'feature'));
