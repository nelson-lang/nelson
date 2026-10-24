%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.checkerror('imref2d([2.5 3])', sprintf(_('%s must contain positive integer values.'), 'Image size'));
%=============================================================================
asserts.checkerror('imref2d([2 3], [1 1], [0 2])', sprintf(_('%s must contain increasing finite values.'), 'XWorldLimits'));
%=============================================================================
asserts.checkerror('imref2d([2 3], 0, 1)', sprintf(_('%s must be a positive finite scalar.'), 'PixelExtentInWorldX'));
%=============================================================================
asserts.checkerror('imref2d([2 3], 1)', _('imref2d requires image size only or both X and Y world arguments.'));
