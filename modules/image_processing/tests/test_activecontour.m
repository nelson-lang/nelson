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
asserts.checkerror('activecontour([1 2], logical([1; 0]))', sprintf(_('%s must have the same size as %s.'), 'Initial mask', 'the image'));
%=============================================================================
asserts.checkerror('activecontour(zeros(2, 2, 2), false(2, 2))', sprintf(_('%s must be a 2-D, RGB, or RGBA array.'), 'Image'));
%=============================================================================
asserts.checkerror('activecontour(zeros(2, 2, 3), false(2, 2, 3))', sprintf(_('%s must have the same size as %s.'), 'Initial mask', 'the image'));
%=============================================================================
asserts.checkerror('activecontour([1 2], [1 2])', sprintf(_('%s must contain binary values.'), 'Initial mask'));
%=============================================================================
asserts.checkerror('activecontour([1 2], logical([1 0]), -1)', sprintf(_('%s must be a nonnegative integer scalar.'), 'Number of iterations'));
%=============================================================================
asserts.checkerror('activecontour([1 2], logical([1 0]), 1, ''unknown'')', sprintf(_('Unsupported "%s" option.'), 'active contour method'));
%=============================================================================
asserts.checkerror('activecontour([1 Inf], logical([1 0]))', sprintf(_('%s values must be finite.'), 'Image'));
%=============================================================================
asserts.checkerror('activecontour([1 + i], logical(1))', sprintf(_('%s must be real.'), 'Image'));
%=============================================================================
asserts.checkerror('activecontour([1 2], logical([1 0]), 1, 2)', sprintf(_('%s must be a string.'), 'Active contour method'));
%=============================================================================
asserts.checkerror('activecontour([1 2], logical([1 0]), ''SmoothFactor'')', _('Name-value arguments must be paired.'));
%=============================================================================
asserts.checkerror('activecontour([1 2], logical([1 0]), 1, 2, 3)', sprintf(_('%s must be a string.'), 'Active contour method'));
%=============================================================================
asserts.checkerror('activecontour([1 2], logical([1 0]), ''Method'', 1)', sprintf(_('%s must be a string.'), 'Active contour method'));
%=============================================================================
asserts.checkerror('activecontour([1 2], logical([1 0]), ''Method'', ''unknown'')', sprintf(_('Unsupported "%s" option.'), 'active contour method'));
%=============================================================================
asserts.checkerror('activecontour([1 2], logical([1 0]), ''Iterations'', -1)', ...
  sprintf(_('%s must be a nonnegative integer scalar.'), 'Number of iterations'));
%=============================================================================
asserts.checkerror('activecontour(zeros(2, 2, 3), false(2, 2), ''edge'')', ...
  _('Edge method is not supported for RGB or RGBA images.'));
%=============================================================================
asserts.checkerror('activecontour([1 2], logical([1 0]), ''Unknown'', true)', sprintf(_('Unsupported "%s" option.'), 'active contour'));
%=============================================================================
asserts.checkerror('activecontour([1 2], logical([1 0]), ''SmoothFactor'', -1)', sprintf(_('%s must be a nonnegative finite scalar.'), 'Smooth factor'));
%=============================================================================
asserts.checkerror('activecontour([1 2], logical([1 0]), ''SmoothFactor'', [1 2])', sprintf(_('%s must be a nonnegative finite scalar.'), 'Smooth factor'));
%=============================================================================
asserts.checkerror('activecontour([1 2], logical([1 0]), ''ContractionBias'', 2)', ...
  sprintf(_('%s must be a finite scalar in the range [%s].'), 'Contraction bias', '-1, 1'));
%=============================================================================
asserts.checkerror('activecontour([1 2], logical([1 0]), ''ContractionBias'', Inf)', ...
  sprintf(_('%s must be a finite scalar in the range [%s].'), 'Contraction bias', '-1, 1'));
%=============================================================================
asserts.checkerror('activecontour([1 2], [1 + i 0])', sprintf(_('%s must be real.'), 'Initial mask'));
%=============================================================================
asserts.checkerror('activecontour([1 2], sparse([1 0]))', sprintf(_('%s are not supported.'), 'Sparse masks'));
%=============================================================================
asserts.checkerror('activecontour([1 2], [1 NaN])', sprintf(_('%s must contain finite values.'), 'Initial mask'));
%=============================================================================
asserts.checkerror('activecontour([1 2], single([1 NaN]))', sprintf(_('%s must contain finite values.'), 'Initial mask'));
%=============================================================================
asserts.checkerror('activecontour([1 2], logical([1 0]), ''Method'', ''edge'', 1, 2)', sprintf(_('%s option name must be a string.'), 'activecontour'));
