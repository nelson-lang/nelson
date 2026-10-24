%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('reshape'), -1);
asserts.isequal(nargout('reshape'), 1);
%=============================================================================
asserts.isequal(reshape(1:6, 2, 3), [1 3 5; 2 4 6]);
asserts.isequal(reshape(1:6, [3 2]), [1 4; 2 5; 3 6]);
asserts.isequal(size(reshape(1:8, 2, 2, 2)), [2 2 2]);
%=============================================================================
A = reshape(1:6, 2, 3);
asserts.isequal(reshape(A, 3, 2), [1 4; 2 5; 3 6]);
asserts.isequal(size(reshape([], 0, 3)), [0 3]);
asserts.checkerror('reshape(1:3, 2, 2)', _('Reshape operation cannot change the number of elements in array.'));
%=============================================================================
