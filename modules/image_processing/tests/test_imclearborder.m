%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.checkerror('imclearborder({1})', sprintf(_('%s must be numeric or logical.'), 'Image'));
%=============================================================================
asserts.checkerror('imclearborder(sparse([1 0]))', sprintf(_('%s are not supported.'), 'Sparse images'));
%=============================================================================
asserts.checkerror('imclearborder([1 + i])', sprintf(_('%s must be real.'), 'Image'));
%=============================================================================
asserts.checkerror('imclearborder(zeros(2, 2, 2))', sprintf(_('%s must be a 2-D array.'), 'Image'));
%=============================================================================
asserts.checkerror('imclearborder([1 Inf])', sprintf(_('%s values must be finite.'), 'Image'));
%=============================================================================
asserts.checkerror('imclearborder(single([1 NaN]))', sprintf(_('%s values must be finite.'), 'Image'));
%=============================================================================
asserts.checkerror('imclearborder([1 2])', sprintf(_('%s must contain binary values.'), 'Image'));
%=============================================================================
asserts.checkerror('imclearborder([1 0], eye(3))', _('Unsupported connectivity.'));
