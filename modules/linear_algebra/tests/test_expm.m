%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--REUSE PROCESS-->
asserts.isequal(nargin('expm'), 1);
asserts.isequal(nargout('expm'), 1);
%=============================================================================
R = expm([1, 2; 3, 4]);
REF = [51.9690, 74.7366; 112.1048, 164.0738];
asserts.isapprox(R, REF, 1e-4);
asserts.isequal(class(R), 'double');
%=============================================================================
R = expm(single([1, 2; 3, 4]));
REF = [51.9689, 74.7365; 112.1048, 164.0737];
asserts.isapprox(R, REF, 1e-4);
asserts.isequal(class(R), 'single');
%=============================================================================
R = expm([]);
REF = [];
asserts.isequal(R, REF);
%=============================================================================
R = expm(100);
REF = exp(100);
asserts.isequal(R, REF);
%=============================================================================
R = expm(NaN);
REF = NaN;
asserts.isequal(R, REF);
%=============================================================================
R = expm(Inf);
REF = NaN;
asserts.isequal(R, REF);
%=============================================================================
R = expm(-Inf);
REF = NaN;
asserts.isequal(R, REF);
%=============================================================================
R = expm(i);
REF = 0.5403 + 0.8415i;
asserts.isapprox(R, REF, 1e-4);
%=============================================================================
M = [1 -1 -1; 0 1 -1; 0 0 1];
R = expm(M);
REF = [2.7183 -2.7183 -1.3591; 0 2.7183 -2.7183; 0 0 2.7183];
asserts.isapprox(R, REF, 1e-4);
%=============================================================================
M = [1 -1; 0 1];
R = expm(M);
REF = [2.7183, -2.7183; 0 2.7183];
%=============================================================================
M = eye(100);
R = expm(M);
REF = 2.7183 * M;
asserts.isapprox(R, REF, 1e-4);
%=============================================================================
M = sparse([1 -1; 0 1]);
R = expm(M);
REF = [2.7183, -2.7183; 0 2.7183];
asserts.isapprox(R, REF, 1e-4);
%=============================================================================
R = expm(sparse([]));
asserts.isequal(R, []);
asserts.isfalse(issparse(R));
%=============================================================================
R = expm(complex(diag([1 2]), 0));
REF = diag([exp(1) exp(2)]);
asserts.isapprox(R, REF, 1e-12);
asserts.isequal(class(R), 'double');
asserts.istrue(isreal(R));
%=============================================================================
R = expm(complex(single(diag([1 2])), single(0)));
REF = single(diag([exp(1) exp(2)]));
asserts.isapprox(R, REF, 1e-6);
asserts.isequal(class(R), 'single');
asserts.istrue(isreal(R));
%=============================================================================
R = expm(complex(single(2), single(0)));
asserts.isequal(R, expm(single(2)));
asserts.istrue(isreal(R));
%=============================================================================
R = expm(complex(single([1 2; 3 4]), single(0)));
asserts.isequal(R, expm(single([1 2; 3 4])));
asserts.isequal(class(R), 'single');
asserts.istrue(isreal(R));
%=============================================================================
M = [1 1 0; 0 0 2; 0 0 -1] + i;
R = expm(M);
REF = [-0.0190 + 0.7636i, -1.0190 + 0.7636i, -1.6511 + 0.7636i;
  -1.9072 + 0.0021i, -0.9072 + 0.0021i, -0.6430 + 0.0021i;
  -0.7649 - 0.3821i, -0.7649 - 0.3821i, -0.3970 - 0.3821i];
asserts.isapprox(R, REF, 1e-4);
%=============================================================================
asserts.checkerror('expm([1 2])', sprintf(_('%s expected.'), 'Square matrix'));
%=============================================================================
asserts.isapprox(expm([0 1; -1 0]), [cos(1) sin(1); -sin(1) cos(1)], 1e-12);
%=============================================================================
% Defective/singular matrices must use the scaling-and-squaring method
% (an eigenvector-based path fails when the eigenvector matrix is singular).
asserts.isapprox(expm([0 1; 0 0]), [1 1; 0 1], 1e-12);
asserts.isapprox(expm([2 1; 0 2]), exp(2) * [1 1; 0 1], 1e-10);
asserts.isapprox(expm([i 1; 0 i]), exp(i) * [1 1; 0 1], 1e-12);
asserts.isapprox(expm(zeros(2)), eye(2), 1e-12);
%=============================================================================
