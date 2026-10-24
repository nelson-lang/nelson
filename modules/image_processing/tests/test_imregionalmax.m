%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.checkerror('imregionalmax({1})', sprintf(_('%s must be numeric or logical.'), 'Image'));
%=============================================================================
asserts.checkerror('imregionalmax([1 + i])', sprintf(_('%s must be real.'), 'Image'));
%=============================================================================
asserts.checkerror('imregionalmax([1 Inf])', sprintf(_('%s values must be finite.'), 'Image'));
%=============================================================================
asserts.checkerror('imregionalmax(sparse([1 0]))', sprintf(_('%s are not supported.'), 'Sparse images'));
%=============================================================================
asserts.checkerror('imregionalmax(zeros(2, 2, 3))', sprintf(_('%s must be a 2-D array.'), 'Image'));
%=============================================================================
asserts.checkerror('imregionalmax([1 2], eye(3))', _('Unsupported connectivity.'));
