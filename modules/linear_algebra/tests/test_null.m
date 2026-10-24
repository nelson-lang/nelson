%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('null'), 2);
asserts.isequal(nargout('null'), 1);
%=============================================================================
% orthonormal null space (svd based)
A = [1 2 3; 4 5 6; 7 8 9];
Z = null(A);
asserts.isequal(size(Z), [3 1]);
asserts.istrue(norm(A * Z) < 1e-10);
asserts.istrue(norm(Z' * Z - eye(size(Z, 2))) < 1e-12);
%=============================================================================
% full rank square matrix has empty null space
asserts.isequal(null(eye(3)), zeros(3, 0));
% magic(4) is rank 3, so its null space has dimension 1
asserts.isequal(size(null(magic(4))), [4 1]);
%=============================================================================
% rational basis
Zr = null([1 2 3], 'r');
asserts.isequal(size(Zr), [3 2]);
asserts.istrue(norm([1 2 3] * Zr) < 1e-12);
asserts.isequal(Zr, [-2 -3; 1 0; 0 1]);
%=============================================================================
% single vector null of a rank-deficient matrix
B = [1 1; 1 1];
Zb = null(B);
asserts.isequal(size(Zb), [2 1]);
asserts.istrue(norm(B * Zb) < 1e-12);
%=============================================================================
asserts.checkerror('null([1 2 3], ''x'')', _('Second argument must be ''r''.'), 'Nelson:null:invalidOption');
%=============================================================================
