%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.checkerror('imregcorr([1 2], [1; 2])', sprintf(_('%s must have the same size.'), 'Moving and fixed images'));
%=============================================================================
asserts.checkerror('imregcorr({1}, [1])', sprintf(_('%s must be numeric or logical.'), 'Image'));
%=============================================================================
asserts.checkerror('imregcorr(sparse([1 0]), [1 0])', sprintf(_('%s are not supported.'), 'Sparse images'));
%=============================================================================
asserts.checkerror('imregcorr([1 + i], [1])', sprintf(_('%s must be real.'), 'Image'));
%=============================================================================
asserts.checkerror('imregcorr(zeros(2, 2, 2), zeros(2, 2, 2))', sprintf(_('%s must be a 2-D or RGB array.'), 'Image'));
%=============================================================================
asserts.checkerror('imregcorr([NaN], [1])', sprintf(_('%s values must be finite.'), 'Image'));
%=============================================================================
asserts.checkerror('imregcorr([1 2], [1 2], 1)', sprintf(_('%s must be a string.'), 'Registration transformation type'));
%=============================================================================
asserts.checkerror('imregcorr([1 2], [1 2], ''projective'')', sprintf(_('Unsupported "%s" option.'), 'registration transformation type'));
