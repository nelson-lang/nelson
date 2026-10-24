%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.checkerror('imreconstruct({1}, 1)', sprintf(_('%s must be numeric or logical.'), 'Image'));
%=============================================================================
asserts.checkerror('imreconstruct([1 2], [1; 2])', sprintf(_('%s must have the same size as %s.'), 'Marker image', 'the mask image'));
%=============================================================================
asserts.checkerror('imreconstruct([1 + i], [2])', sprintf(_('%s must be real.'), 'Marker and mask images'));
%=============================================================================
asserts.checkerror('imreconstruct([1 Inf], [2 3])', sprintf(_('%s values must be finite.'), 'Marker and mask image'));
%=============================================================================
asserts.checkerror('imreconstruct([2], [1])', _('Marker image values must be less than or equal to mask image values.'));
%=============================================================================
asserts.checkerror('imreconstruct(sparse([1 0]), [1 1])', sprintf(_('%s are not supported.'), 'Sparse images'));
%=============================================================================
asserts.checkerror('imreconstruct([1 2], [2 2], eye(3))', _('Unsupported connectivity.'));
