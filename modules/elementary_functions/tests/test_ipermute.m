%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('ipermute'), -1);
assert_isequal(nargout('ipermute'), -1);
%=============================================================================
rng default
B = rand(4, 3, 2);
A = ipermute(B,[3 1 2]);
assert_isequal(size(A), [ 3     2     4]);
C = permute(A,[3 1 2]);
assert_isequal(B, C);
%=============================================================================
x = [1 2 3; 4 5 6];
y = permute(x,[3 1 2]),
x2 = ipermute(y,[3 1 2]);
assert_isequal(x, x2);