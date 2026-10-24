%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
A = sparse([1 0 2 0; 0 3 0 4; 5 0 6 0]);
B = sparse([10; 20; 30]);
BF = repmat(full(B), 1, 4);
%=============================================================================
R = A + B;
asserts.istrue(issparse(R));
asserts.isequal(R, sparse(full(A) + BF));
%=============================================================================
R = A .* B;
asserts.istrue(issparse(R));
asserts.isequal(R, sparse(full(A) .* BF));
%=============================================================================
R = A ./ B;
asserts.istrue(issparse(R));
asserts.isequal(R, sparse(full(A) ./ BF));
%=============================================================================
R = A .^ B;
asserts.istrue(issparse(R));
asserts.isequal(R, sparse(full(A) .^ BF));
%=============================================================================
AS = sparse(single([1 0 2 0; 0 3 0 4; 5 0 6 0]));
BS = sparse(single([1; 2; 3]));
BSF = repmat(full(BS), 1, 4);
R = AS .^ BS;
asserts.istrue(issparse(R));
asserts.isequal(class(R), 'single');
asserts.isapprox(full(R), full(AS) .^ BSF, 1e-6);
%=============================================================================
R = A > B;
asserts.istrue(issparse(R));
asserts.isequal(R, sparse(full(A) > BF));
%=============================================================================
D = zeros(3, 4, 2);
D(:, :, 1) = 10;
D(:, :, 2) = 20;
R = A + D;
REF = zeros(3, 4, 2);
for k = 1:2
  REF(:, :, k) = full(A) + D(:, :, k);
end
asserts.isfalse(issparse(R));
asserts.isequal(size(R), [3 4 2]);
asserts.isequal(R, REF);
%=============================================================================
