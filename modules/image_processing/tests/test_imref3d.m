%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.checkerror('imref3d([2 3])', sprintf(_('%s must contain at least three dimensions.'), 'Image size'));
%=============================================================================
asserts.checkerror('imref3d([2 3 4], [0 1], [0 1], [2 2])', sprintf(_('%s must contain increasing finite values.'), 'ZWorldLimits'));
%=============================================================================
asserts.checkerror('imref3d([2 3 4], 1, 2, Inf)', sprintf(_('%s must be a positive finite scalar.'), 'PixelExtentInWorldZ'));
%=============================================================================
asserts.checkerror('imref3d([2 3 4], 1, 2)', _('imref3d requires image size only or X, Y and Z world arguments.'));
