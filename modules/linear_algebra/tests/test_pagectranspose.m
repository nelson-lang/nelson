%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('pagectranspose'), 1);
asserts.isequal(nargout('pagectranspose'), 1);
%=============================================================================
X = reshape(1:24, 2, 3, 4);
Y = pagectranspose(X);
asserts.isequal(size(Y), [3 2 4]);
for k = 1:4
  asserts.isequal(Y(:, :, k), X(:, :, k)');
end
%=============================================================================
% complex: conjugate transpose
Z = reshape((1:8) + 1i * (8:-1:1), 2, 2, 2);
W = pagectranspose(Z);
for k = 1:2
  asserts.isequal(W(:, :, k), Z(:, :, k)');
end
%=============================================================================
% equals conj of pagetranspose
asserts.isequal(pagectranspose(Z), conj(pagetranspose(Z)));
%=============================================================================
% 2-D input matches '
asserts.isequal(pagectranspose([1 + 2i 3; 4 5 - 1i]), [1 + 2i 3; 4 5 - 1i]');
%=============================================================================
