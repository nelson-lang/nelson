%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
asserts.isequal(nargin('bsxfun'), 3);
asserts.isequal(nargout('bsxfun'), 1);
%=============================================================================
asserts.isequal(bsxfun(@plus, [1; 2; 3], [10 20]), [11 21; 12 22; 13 23]);
asserts.isequal(bsxfun(@times, [1 2 3], 2), [2 4 6]);
asserts.isequal(bsxfun(@minus, [1 2 3], [1 2 3]), [0 0 0]);
A = magic(3);
asserts.isequal(bsxfun(@minus, A, mean(A)), A - mean(A));
asserts.isequal(bsxfun(@rdivide, [2 4 6], 2), [1 2 3]);
asserts.isequal(bsxfun(@max, [1 5 3], [4 2 6]), [4 5 6]);
% singleton expansion on both operands
asserts.isequal(bsxfun(@plus, (1:3)', 1:4), (1:3)' + (1:4));
% string function name accepted
asserts.isequal(bsxfun('plus', 1, 2), 3);
%=============================================================================
asserts.checkerror('bsxfun(@plus, ones(2, 3), ones(4, 5))', _('Non-singleton dimensions of the two input arrays must match each other.'), 'Nelson:bsxfun:arrayDimensionsMustMatch');
%=============================================================================
