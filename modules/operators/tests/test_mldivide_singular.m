%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Singular square systems keep the LU result (Inf/NaN), not a least-squares answer.
lastwarn('', '');
X = [1 2; 2 4] \ [1; 1];
[msg, id] = lastwarn();
asserts.istrue(isequaln(X, [-Inf; Inf]));
asserts.isequal(id, 'Nelson:singularMatrix');
asserts.isequal(msg, _('Matrix is singular to working precision.'));
%=============================================================================
X = [1 2; 2 4] \ [1 0; 1 2];
asserts.istrue(isequaln(X, [-Inf Inf; Inf -Inf]));
%=============================================================================
X = [1 2; 2 4] \ [1; 2];
asserts.istrue(isequaln(X, [NaN; NaN]));
%=============================================================================
Y = [1 1] / [1 2; 2 4].';
asserts.istrue(isequaln(Y, [-Inf Inf]));
%=============================================================================
% Symmetric singular matrix: Cholesky gives way to LU.
lastwarn('', '');
X = [2 2; 2 2] \ [1; 3];
[msg, id] = lastwarn();
asserts.istrue(isequaln(X, [-Inf; Inf]));
asserts.isequal(id, 'Nelson:singularMatrix');
%=============================================================================
% Zero and singular diagonal matrices use triangular substitution.
asserts.istrue(isequaln(zeros(2) \ [1; 1], [Inf; NaN]));
asserts.istrue(isequaln([1 1] / zeros(2), [NaN Inf]));
asserts.istrue(isequaln(zeros(3) \ [1; 2; 3], [Inf; NaN; NaN]));
asserts.istrue(isequaln([1 2 3] / zeros(3), [NaN NaN Inf]));
asserts.istrue(isequaln(diag([0 1]) \ [1; 1], [Inf; NaN]));
asserts.istrue(isequaln([1 1] / diag([0 1]), [Inf 1]));
asserts.istrue(isequaln(diag([1 0]) \ [1; 1], [1; Inf]));
asserts.istrue(isequaln([1 1] / diag([1 0]), [NaN Inf]));
%=============================================================================
% Triangular matrices.
asserts.istrue(isequaln([1 2; 0 0] \ [1; 1], [-Inf; Inf]));
asserts.istrue(isequaln([1 0; 2 0] \ [1; 1], [1; -Inf]));
asserts.istrue(isequaln([1 2 3; 0 0 1; 0 0 2] \ [1; 1; 1], [-Inf; Inf; 0.5]));
asserts.istrue(isequaln([1 1 1] / [1 2 3; 0 0 1; 0 0 2].', [-Inf Inf 0.5]));
lastwarn('', '');
X = [1 1; 0 1e-20] \ [1; 1];
[msg, id] = lastwarn();
asserts.isequal(X, [-1e20; 1e20]);
asserts.isequal(id, 'Nelson:nearlySingularMatrix');
asserts.isequal([1 0; 1 1e-20] \ [1; 1], [1; 0]);
% Triangular RCOND is the diagonal ratio min|d| / max|d|.
lastwarn('', '');
X = [1 -1; 0 4.4408920985006262e-16] \ [1; 1];
[msg, id] = lastwarn();
asserts.isequal(id, '');
lastwarn('', '');
X = [1 -1; 0 2e-16] \ [1; 1];
[msg, id] = lastwarn();
asserts.isequal(id, 'Nelson:nearlySingularMatrix');
asserts.istrue(contains(msg, 'RCOND = 2.000000e-16'));
%=============================================================================
% Upper Hessenberg singular matrix.
asserts.istrue(isequaln([1 2 3; 4 5 6; 0 0 0] \ [1; 1; 1], [NaN; -Inf; Inf]));
%=============================================================================
% Single precision.
X = single([1 2; 2 4]) \ single([1; 1]);
asserts.isequal(class(X), 'single');
asserts.istrue(isequaln(X, single([-Inf; Inf])));
X = single([1 2; 2 4]) \ single([1; 2]);
asserts.istrue(isequaln(X, single([NaN; NaN])));
%=============================================================================
% Complex singular matrix.
X = [1 2i; 2 4i] \ [1; 1];
asserts.istrue(isequaln(real(X), [NaN; Inf]));
asserts.istrue(isequaln(imag(X), [NaN; NaN]));
X = single([1 2i; 2 4i]) \ single([1; 1]);
asserts.isequal(class(X), 'single');
asserts.istrue(isequaln(real(X), single([NaN; Inf])));
%=============================================================================
% Inf or NaN in the matrix.
lastwarn('', '');
X = [1 Inf; 2 3] \ [1; 1];
[msg, id] = lastwarn();
asserts.isequal(X, [0.5; 0]);
asserts.isequal(id, 'Nelson:singularMatrix');
lastwarn('', '');
X = [1 NaN; 2 3] \ [1; 1];
[msg, id] = lastwarn();
asserts.istrue(isequaln(X, [NaN; NaN]));
asserts.isequal(id, 'Nelson:illConditionedMatrix');
%=============================================================================
% Nearly singular square matrix keeps the LU answer with a warning.
lastwarn('', '');
X = [1 2 3; 4 5 6; 7 8 9] \ [1; 2; 3];
[msg, id] = lastwarn();
asserts.isequal(id, 'Nelson:nearlySingularMatrix');
asserts.istrue(startsWith(msg, _('Matrix is nearly singular or badly scaled')));
asserts.isapprox([1 2 3; 4 5 6; 7 8 9] * X, [1; 2; 3], 1e-12);
%=============================================================================
