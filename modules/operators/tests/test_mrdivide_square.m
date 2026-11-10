%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Dense square divisor: B / A factors A itself and matches (A' \ B')'.
warning('off', 'Nelson:nearlySingularMatrix');
warning('off', 'Nelson:singularMatrix');
warning('off', 'Nelson:illConditionedMatrix');
%=============================================================================
A = [1 2 3; 4 5 6; 7 8 10];
B = [1 2 3; 3 1 2];
lastwarn('');
X = B / A;
asserts.isequal(lastwarn(), '');
asserts.isequal(size(X), [2 3]);
asserts.isapprox(X * A, B, 1e-12);
asserts.isapprox(X, (A' \ B')', 1e-14);
%=============================================================================
% general LU, row vector and matrix numerators
A = [1 2 3; 4 5 6; 7 8 10];
asserts.isapprox([1 2 3] / A, [1 0 0], 1e-14);
X = [1 2 3; 3 1 2] / [2 0 0; 1 3 0; 4 5 6];
REF = [-0.416666666666667 -0.166666666666667 0.5; 0.944444444444444 -0.222222222222222 0.333333333333333];
asserts.isapprox(X, REF, 1e-14);
X = [1 2 3; 3 1 2] / [2 1 4; 0 3 5; 0 0 6];
REF = [0.5 0.5 -0.25; 1.5 -0.166666666666667 -0.527777777777778];
asserts.isapprox(X, REF, 1e-14);
%=============================================================================
% symmetric positive definite and symmetric indefinite divisors
X = [1 2 3] / [4 1 0; 1 5 2; 0 2 6];
asserts.isapprox(X, [0.204081632653061 0.183673469387755 0.438775510204082], 1e-14);
X = [1 2 3] / [1 2 0; 2 1 0; 0 0 1];
asserts.isapprox(X, [1 0 3], 1e-14);
%=============================================================================
% singular and badly scaled divisors keep their warnings
lastwarn('');
X = [1 2 3] / [0 1 4; 0 3 5; 0 0 6];
[~, id] = lastwarn();
asserts.isequal(id, 'Nelson:singularMatrix');
asserts.isequal(X(1:2), [Inf -Inf]);
asserts.istrue(isnan(X(3)));
lastwarn('');
X = [1 2 3] / [2 0 0; 1 0 0; 4 5 6];
[~, id] = lastwarn();
asserts.isequal(id, 'Nelson:singularMatrix');
asserts.isequal(X, [Inf -Inf 0.5]);
lastwarn('');
X = [1 2 3] / diag([1e-20 1 1]);
[~, id] = lastwarn();
asserts.isequal(id, 'Nelson:nearlySingularMatrix');
asserts.isapprox(X, [1e20 2 3], 1e-14);
lastwarn('');
X = [1 2 3] / [1e-20 0 1; 0 1 0; 0 0 1];
[~, id] = lastwarn();
asserts.isequal(id, 'Nelson:nearlySingularMatrix');
asserts.isapprox(X, [1e20 2 -1e20], 1e-14);
lastwarn('');
X = [1 2 3] / [1 NaN 3; 4 5 6; 7 8 10];
[~, id] = lastwarn();
asserts.isequal(id, 'Nelson:illConditionedMatrix');
asserts.isequal(isnan(X), [true true true]);
lastwarn('');
X = [1 2] / [1 1; 1 1 + 1e-17];
[~, id] = lastwarn();
asserts.isequal(id, 'Nelson:singularMatrix');
asserts.isequal(X, [-Inf Inf]);
lastwarn('');
X = [1 2 3; 4 5 6] / [1 2 3; 4 5 6; 7 8 9];
[~, id] = lastwarn();
asserts.isequal(id, 'Nelson:nearlySingularMatrix');
asserts.isapprox(X * [1 2 3; 4 5 6; 7 8 9], [1 2 3; 4 5 6], 1e-12);
X = [1 2 3] / ([1 2 3; 4 5 6; 7 8 9] + 1e-13 * [1 0 0; 0 0 0; 0 0 0]);
asserts.isapprox(X, [0 2 -1], 1e-12);
%=============================================================================
% complex divisors and numerators
X = [1+2i 2 3-1i] / [1 2i 3; 4 5 6; 7 8i 10];
REF = [0.921801361494152-2.45575144004189i, 0.430092511782161+0.00663292023040674i, ...
  -0.234595915517542+0.632745679874323i];
asserts.isapprox(X, REF, 1e-13);
X = [1+2i 2 3-1i; 1 1 1] / [4 1+1i 0; 1-1i 5 2; 0 2 6];
REF = [0.195652173913043+0.608695652173913i, 0.326086956521739-0.108695652173913i, ...
  0.391304347826087-0.130434782608696i; ...
  0.239130434782609+0.0434782608695652i, 0.108695652173913-0.0652173913043478i, ...
  0.130434782608696+0.0217391304347826i];
asserts.isapprox(X, REF, 1e-13);
X = [1 2 3] / [2 0 0; 1i 3 0; 4 5 6];
asserts.isapprox(X, [-0.5+0.0833333333333333i, -0.166666666666667, 0.5], 1e-14);
X = [1+1i 2 3] / [1 2 3; 4 5 6; 7 8 10];
asserts.isapprox(X, [1-0.666666666666667i, -1.33333333333333i, 1i], 1e-13);
%=============================================================================
% a complex quotient without imaginary part is real
X = complex([1 2 3]) / complex([1 2 3; 4 5 6; 7 8 10]);
asserts.istrue(isreal(X));
asserts.isapprox(X, [1 0 0], 1e-14);
%=============================================================================
% single
X = single([1 2 3]) / single([1 2 3; 4 5 6; 7 8 10]);
asserts.isequal(class(X), 'single');
asserts.isapprox(X, single([1 0 0]), 1e-5);
X = single([1 2 3]) / single([1 2i 3; 4 5 6; 7 8 10]);
asserts.isequal(class(X), 'single');
asserts.isapprox(X, single([0.323076963424683+0.184615403413773i, ...
  3.72307872772217-1.01538515090942i, -2.03077030181885+0.553846478462219i]), 1e-5);
%=============================================================================
% mixed classes, sparse, scalar and empty operands keep their route
X = single([1 2 3]) / [1 2 3; 4 5 6; 7 8 10];
asserts.isequal(class(X), 'single');
X = [1 2 3] / sparse([1 2 3; 4 5 6; 7 8 10]);
asserts.isapprox(full(X), [1 0 0], 1e-14);
asserts.isequal([2 4 6] / 2, [1 2 3]);
asserts.isequal(size(zeros(0, 3) / [1 2 3; 4 5 6; 7 8 10]), [0 3]);
asserts.isequal(size(ones(2, 0) / zeros(3, 0)), [2 3]);
msg = _('Requested divide operation requires arguments to have correct dimensions.');
asserts.checkerror('[1 2] / [1 2 3; 4 5 6; 7 8 10]', msg);
%=============================================================================
% larger random systems agree with the left division
A = magic(6) + 10 * eye(6);
B = reshape(1:24, 4, 6);
asserts.isapprox(B / A, (A' \ B')', 1e-13);
asserts.isapprox((B + 1i) / (A + 2i * eye(6)), ((A + 2i * eye(6))' \ (B + 1i)')', 1e-13);
%=============================================================================
