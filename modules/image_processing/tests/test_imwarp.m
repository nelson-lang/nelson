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
asserts.checkerror('imwarp({1 2}, eye(3))', sprintf(_('%s must be numeric or logical.'), 'Image'));
%=============================================================================
asserts.checkerror('imwarp(sparse([1 2]), eye(3))', sprintf(_('%s are not supported.'), 'Sparse images'));
%=============================================================================
asserts.checkerror('imwarp(zeros(2, 2, 5), eye(3))', sprintf(_('%s must be a 2-D, RGB, or RGBA array.'), 'Image'));
%=============================================================================
asserts.checkerror('imwarp([1 2], ''abc'')', _('Transformation matrix must be numeric and real.'));
%=============================================================================
asserts.checkerror('imwarp([1 2], struct(''Type'', ''affine''))', _('Transformation structure must contain a T field.'));
%=============================================================================
asserts.checkerror('imwarp([1 2], eye(2))', sprintf(_('%s must be %d-by-%d or %d-by-%d.'), 'Transformation matrix', 3, 3, 2, 3));
%=============================================================================
asserts.checkerror('imwarp([1 2], eye(3) + i)', _('Transformation matrix must be numeric and real.'));
%=============================================================================
asserts.checkerror('imwarp([1 2], [1 0 0; 0 0 0; 0 0 1])', _('Transformation matrix must be finite and nonsingular.'));
%=============================================================================
asserts.checkerror('imwarp(zeros(2, 2, 3), eye(3), ''FillValues'', [1 2])', _('Fill values must be scalar or match the number of channels.'));
%=============================================================================
asserts.checkerror('imwarp([1 2], eye(3), ''FillValues'', NaN)', sprintf(_('%s values must be finite.'), 'Fill'));
%=============================================================================
asserts.checkerror('imwarp([1 2], eye(3), ''FillValues'', ''x'')', _('Fill values must be numeric or logical and real.'));
%=============================================================================
asserts.checkerror('imwarp([1 2], imref2d([2 2]), eye(3))', _('Spatial reference image size must match image size.'));
%=============================================================================
asserts.checkerror('imwarp([1 2], eye(3), ''OutputView'', [2.5 2])', sprintf(_('%s must contain positive integer values.'), 'Image size'));
%=============================================================================
asserts.checkerror('imwarp([1 2], eye(3), ''OutputView'', struct(''ImageSize'', [1 2]))', _('Spatial reference must contain ImageSize, XWorldLimits and YWorldLimits fields.'));
%=============================================================================
asserts.checkerror('imwarp([1 2], eye(3), ''OutputView'', ''bad'')', sprintf(_('Unsupported "%s" option.'), 'output view'));
%=============================================================================
asserts.checkerror('imwarp([1 2], eye(3), 1, 2)', sprintf(_('%s option name must be a string.'), 'imwarp'));
%=============================================================================
asserts.checkerror('imwarp([1 2], eye(3), ''Interpolation'', 1)', sprintf(_('%s must be a string.'), 'Interpolation method'));
%=============================================================================
asserts.checkerror('imwarp([1 2], eye(3), ''Interpolation'')', _('Name-value arguments must be paired.'));
%=============================================================================
asserts.checkerror('imwarp([1 2], eye(3), ''Unknown'', true)', sprintf(_('Unsupported "%s" option.'), 'imwarp'));
%=============================================================================
asserts.checkerror('imwarp([1 2], affine3d(eye(4)))', sprintf(_('%s must be a 3-D array.'), 'Volume'));
%=============================================================================
asserts.isequal(imwarp(cat(3, [1 2], [3 + i 4]), affine3d(eye(4)), 'nearest'), cat(3, [1 2], [3 + i 4]));
%=============================================================================
asserts.checkerror('imwarp(ones(2, 2, 2), eye(3))', sprintf(_('%s must be a 2-D, RGB, or RGBA array.'), 'Image'));
%=============================================================================
asserts.checkerror('imwarp(ones(2, 2, 2), affine3d(eye(4)), ''cubic'')', sprintf(_('Unsupported "%s" option.'), 'interpolation method'));
%=============================================================================
asserts.checkerror('imwarp(ones(2, 2, 2), affine3d(eye(4)), ''FillValues'', NaN)', sprintf(_('%s values must be finite.'), 'Fill'));
%=============================================================================
asserts.checkerror('imwarp(ones(2, 2, 2), affine3d(eye(4)), ''FillValues'', [1 2])', _('Fill values must be scalar for 3-D volumes.'));
%=============================================================================
asserts.checkerror('imwarp(ones(2, 2, 2), affine3d(eye(4)), ''OutputView'', ''bad'')', sprintf(_('Unsupported "%s" option.'), 'output view'));
%=============================================================================
asserts.checkerror('imwarp(ones(2, 2, 2), affine3d(eye(4)), ''OutputView'', [2 2])', ...
  sprintf(_('%s must contain at least three dimensions.'), 'Image size'));
%=============================================================================
asserts.checkerror('imwarp(ones(2, 2, 2), affine3d([1 0 0 1; 0 1 0 0; 0 0 1 0; 0 0 0 1]))', ...
  _('Affine transformation matrix must have [0 0 0 1] as its last column.'));
%=============================================================================
asserts.checkerror('imwarp(ones(2, 2, 2), imref3d([2 2 3]), affine3d(eye(4)))', _('Spatial reference image size must match image size.'));
