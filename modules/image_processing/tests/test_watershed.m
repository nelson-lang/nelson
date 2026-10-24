%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.checkerror('watershed([1 2], 6)', _('Unsupported connectivity.'));
%=============================================================================
asserts.checkerror('watershed([1 2], NaN)', _('Unsupported connectivity.'));
%=============================================================================
asserts.checkerror('watershed([1 2], Inf)', _('Unsupported connectivity.'));
%=============================================================================
asserts.checkerror('watershed([1 2], [4 8])', _('Connectivity must be a numeric/logical scalar or 3-by-3 matrix.'));
%=============================================================================
asserts.checkerror('watershed([1 2], 4 + i)', _('Connectivity must be a numeric/logical scalar or 3-by-3 matrix.'));
%=============================================================================
asserts.checkerror('watershed([1 2], ''4'')', _('Connectivity must be a numeric/logical scalar or 3-by-3 matrix.'));
%=============================================================================
asserts.checkerror('watershed([1 2], eye(3))', _('Unsupported connectivity.'));
%=============================================================================
asserts.checkerror('watershed([1 2], single(eye(3)))', _('Unsupported connectivity.'));
%=============================================================================
asserts.checkerror('watershed([1 2], true(2, 2))', _('Connectivity must be a numeric/logical scalar or 3-by-3 matrix.'));
%=============================================================================
asserts.checkerror('watershed(ones(2, 2, 2), 4)', _('Unsupported connectivity.'));
%=============================================================================
asserts.checkerror('watershed(ones(2, 2, 2), true(3, 3))', _('Connectivity must be a numeric/logical scalar or 3-by-3-by-3 array.'));
%=============================================================================
asserts.checkerror('watershed(ones(2, 2, 2), false(3, 3, 3))', _('Unsupported connectivity.'));
%=============================================================================
asserts.checkerror('watershed([1 Inf])', sprintf(_('%s values must be finite.'), 'Image'));
%=============================================================================
asserts.checkerror('watershed([1 + i])', sprintf(_('%s must be real.'), 'Image'));
%=============================================================================
asserts.checkerror('watershed(sparse([1 0]))', sprintf(_('%s are not supported.'), 'Sparse images'));
%=============================================================================
asserts.checkerror('watershed(zeros(2, 2, 2, 2))', sprintf(_('%s must be a 2-D or 3-D array.'), 'Image'));
