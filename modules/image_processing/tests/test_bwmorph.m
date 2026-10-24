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
asserts.checkerror('bwmorph(logical([1 0]), 1)', sprintf(_('%s must be a string.'), 'Morphological operation'));
%=============================================================================
asserts.checkerror('bwmorph(logical([1 0]), ''unknown'')', sprintf(_('Unsupported "%s" option.'), 'bwmorph operation'));
%=============================================================================
asserts.checkerror('bwmorph(logical([1 0]), ''clean'', -1)', _('Number of iterations must be a nonnegative integer scalar or Inf.'));
%=============================================================================
asserts.checkerror('bwmorph(logical([1 0]), ''clean'', [1 2])', _('Number of iterations must be a nonnegative integer scalar or Inf.'));
%=============================================================================
asserts.checkerror('bwmorph(logical([1 0]), ''clean'', 1 + i)', _('Number of iterations must be a nonnegative integer scalar or Inf.'));
%=============================================================================
asserts.checkerror('bwmorph(logical([1 0]), ''clean'', ''1'')', _('Number of iterations must be a nonnegative integer scalar or Inf.'));
%=============================================================================
asserts.checkerror('bwmorph({1}, ''clean'')', sprintf(_('%s must be numeric or logical.'), 'Image'));
%=============================================================================
asserts.checkerror('bwmorph(sparse([1 0]), ''clean'')', sprintf(_('%s are not supported.'), 'Sparse images'));
%=============================================================================
asserts.checkerror('bwmorph(zeros(2, 2, 3), ''clean'')', sprintf(_('%s must be a 2-D array.'), 'Image'));
%=============================================================================
asserts.checkerror('bwmorph([NaN 0], ''clean'')', _('Conversion to logical with NaN is not possible.'));
%=============================================================================
asserts.checkerror('bwmorph(single([NaN 0]), ''clean'')', _('Conversion to logical with NaN is not possible.'));
