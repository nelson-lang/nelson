%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.checkerror('affine3d([1 0 0 1; 0 1 0 0; 0 0 1 0; 0 0 0 1])', _('Affine transformation matrix must have [0 0 0 1] as its last column.'));
%=============================================================================
asserts.checkerror('affine3d(eye(3))', sprintf(_('%s must be %d-by-%d or %d-by-%d.'), 'Affine transformation matrix', 4, 4, 3, 4));
%=============================================================================
asserts.checkerror('affine3d([1 0 0 0; 0 0 0 0; 0 0 1 0; 0 0 0 1])', _('Transformation matrix must be finite and nonsingular.'));
