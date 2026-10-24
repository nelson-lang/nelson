%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('repmat'), -1)
asserts.isequal(nargout('repmat'), 1)
%=============================================================================
R = repmat(1:3, 2, 2);
REF = [1, 2, 3, 1, 2, 3; 1, 2, 3, 1, 2, 3];
asserts.isequal(R, REF);
%=============================================================================
R = repmat(1:3, 2);
REF = [1, 2, 3, 1, 2, 3; 1, 2, 3, 1, 2, 3];
asserts.isequal(R, REF);
%=============================================================================
R = repmat(1 + 20i, 2);
REF = [1 + 20 * i, 1 + 20 * i; 1 + 20 * i, 1 + 20 * i];
asserts.isequal(R, REF);
%=============================================================================
R = repmat('Nelson', 3, 2);
REF = ['NelsonNelson';
  'NelsonNelson';
  'NelsonNelson']
asserts.isequal(R, REF);
%=============================================================================
R = repmat(eps, 3, 2);
REF = eps * ones(3, 2);
asserts.isequal(R, REF);
%=============================================================================
X = [10 20; 30 40];
R = repmat(X, [2 3 2]);
%=============================================================================
X = [10 20; 30 40];
R = repmat(X, [2 -3 2]);
REF = zeros(4, 0, 2);
asserts.isequal(R, REF);
%=============================================================================
asserts.isequal(repmat([1 2], 2, 1), [1 2; 1 2]);
asserts.isequal(class(repmat(single(2), 2, 3)), 'single');
asserts.isequal(size(repmat(single(2), 2, 3)), [2 3]);
%=============================================================================
% 2-D block replicate (contiguous-memcpy path) matches block indexing
A = reshape(1:12, 3, 4);
ri = mod(0:(3 * 2 - 1), 3) + 1;
ci = mod(0:(4 * 3 - 1), 4) + 1;
asserts.isequal(repmat(A, 2, 3), A(ri, ci));
% row-vector replicated down the rows (M == 1 path)
V = [10 20 30 40];
asserts.isequal(repmat(V, 3, 1), [V; V; V]);
% column-vector replicated across columns
W = [1; 2; 3];
asserts.isequal(repmat(W, 1, 4), [W W W W]);
% class preserved for complex, char and integer inputs
asserts.isequal(class(repmat(single(A), 2, 2)), 'single');
Zc = [1 + 2i 3 - 1i; 0 4i];
asserts.isequal(repmat(Zc, 2, 2), Zc([1 2 1 2], [1 2 1 2]));
asserts.isequal(repmat('ab', 2, 3), ['ababab'; 'ababab']);
asserts.isequal(repmat(int16([1 2; 3 4]), 3, 1), int16([1 2; 3 4; 1 2; 3 4; 1 2; 3 4]));
%=============================================================================
% a zero replication factor with a non-scalar source yields an empty result
% (previously crashed instead of writing into a zero-length buffer)
R = repmat('abc', 1, 0);
asserts.isequal(size(R), [1 0]);
asserts.isequal(class(R), 'char');
%=============================================================================
R = repmat([1 2 3], 1, 0);
asserts.isequal(size(R), [1 0]);
%=============================================================================
R = repmat([1; 2], 2, 0);
asserts.isequal(size(R), [4 0]);
%=============================================================================
R = repmat([1 2; 3 4], 0, 2);
asserts.isequal(size(R), [0 4]);
%=============================================================================
R = repmat(uint8([1 2]), 1, 0);
asserts.isequal(size(R), [1 0]);
asserts.isequal(class(R), 'uint8');
%=============================================================================
