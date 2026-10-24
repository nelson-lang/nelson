%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('spdiags'), 4);
asserts.isequal(nargout('spdiags'), 2);
%=============================================================================
A = sparse([4 0 1; 0 5 0; 2 0 6]);
[B, d] = spdiags(A);
asserts.isequal(B, [2 4 0; 0 5 0; 0 6 1]);
asserts.isequal(d, [-2; 0; 2]);
%=============================================================================
B = [10 20; 30 40; 50 60; 70 80];
R = spdiags(B, [-1 1], 4, 4);
asserts.istrue(issparse(R));
asserts.isequal(full(R), [0 40 0 0; 10 0 60 0; 0 30 0 80; 0 0 50 0]);
%=============================================================================
B = single([(1:4)' (5:8)']);
R = spdiags(B, [0 1], 4, 4);
asserts.istrue(issparse(R));
asserts.isequal(class(R), 'single');
asserts.isequal(full(R), single([1 6 0 0; 0 2 7 0; 0 0 3 8; 0 0 0 4]));
%=============================================================================
B = single([(1:4)' + i * (5:8)' (9:12)' - i * (13:16)']);
R = spdiags(B, [0 -1], 4, 4);
asserts.istrue(issparse(R));
asserts.isequal(class(R), 'single');
REF = single([1 + 5i 0 0 0; 9 - 13i 2 + 6i 0 0; 0 10 - 14i 3 + 7i 0; 0 0 11 - 15i 4 + 8i]);
asserts.isapprox(full(R), REF, 1e-6);
%=============================================================================
A = sparse(single([1 + 2i 0 0; 3 4 - 5i 0; 0 6 7]));
[B, d] = spdiags(A);
asserts.isequal(class(B), 'single');
asserts.isapprox(B, single([3 1 + 2i; 6 4 - 5i; 0 7]), 1e-6);
asserts.isequal(d, [-1; 0]);
%=============================================================================
A = sparse(single(ones(4, 4)));
B = single([(1:4)' + i * (5:8)' (9:12)' - i * (13:16)']);
R = spdiags(B, [0 -1], A);
asserts.istrue(issparse(R));
asserts.isequal(class(R), 'single');
REF = single([1 + 5i 1 1 1; 9 - 13i 2 + 6i 1 1; 1 10 - 14i 3 + 7i 1; 1 1 11 - 15i 4 + 8i]);
asserts.isapprox(full(R), REF, 1e-6);
%=============================================================================
