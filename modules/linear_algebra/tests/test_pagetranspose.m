%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('pagetranspose'), 1);
asserts.isequal(nargout('pagetranspose'), 1);
%=============================================================================
X = reshape(1:24, 2, 3, 4);
Y = pagetranspose(X);
asserts.isequal(size(Y), [3 2 4]);
for k = 1:4
  asserts.isequal(Y(:, :, k), X(:, :, k).');
end
%=============================================================================
% 2-D input matches .'
asserts.isequal(pagetranspose([1 2 3; 4 5 6]), [1 2 3; 4 5 6].');
%=============================================================================
% complex: transpose does NOT conjugate
Z = reshape((1:8) + 1i * (8:-1:1), 2, 2, 2);
asserts.isequal(pagetranspose(Z), permute(Z, [2 1 3]));
%=============================================================================
% preserves class for non-floating types
asserts.isequal(class(pagetranspose(int16(X))), 'int16');
asserts.isequal(pagetranspose(logical([1 0; 0 1])), logical([1 0; 0 1]).');
asserts.isequal(pagetranspose('abc'), 'abc'.');
%=============================================================================
