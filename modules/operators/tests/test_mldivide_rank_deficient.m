%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Rank deficient systems return the basic solution: dependent columns are set to 0.
lastwarn('', '');
X = [1 2; 2 4; 3 6] \ [1; 1; 1];
[msg, id] = lastwarn();
asserts.isapprox(X, [0; 0.2142857142857143], 1e-15);
asserts.isequal(X(1), 0);
asserts.isequal(id, 'Nelson:rankDeficientMatrix');
asserts.isequal(msg, sprintf(_('Matrix is rank deficient, rank = %d, tol = %e.'), 1, 4.984889e-15));
%=============================================================================
X = [1 2; 2 4; 3 6] \ [1 2; 1 0; 1 5];
asserts.isapprox(X, [0 0; 0.2142857142857143 0.6071428571428571], 1e-15);
%=============================================================================
Y = [1 1 1] / [1 2; 2 4; 3 6].';
asserts.isapprox(Y, [0 0.2142857142857143], 1e-15);
%=============================================================================
X = [1 2 3; 2 4 6; 3 6 9; 1 1 1] \ [1; 2; 3; 4];
asserts.isapprox(X, [5.5; 0; -1.5], 1e-14);
asserts.isequal(X(2), 0);
%=============================================================================
X = [1 2 1; 2 4 2; 3 6 4; 0 0 1] \ [1; 2; 3; 4];
asserts.isapprox(X, [0; -1.2894736842105263; 2.9473684210526319], 1e-14);
asserts.isequal(X(1), 0);
%=============================================================================
% Underdetermined systems: full rank and rank deficient.
lastwarn('', '');
X = [1 2 3; 4 5 6] \ [1; 2];
[msg, id] = lastwarn();
asserts.isequal(id, '');
asserts.isapprox(X, [0; 0; 1/3], 1e-14);
asserts.isequal(X(2), 0);
X = [1 2 3; 2 4 6] \ [1; 2];
asserts.isequal(X, [0; 0; 1/3]);
%=============================================================================
% Zero matrices give zero with rank 0.
lastwarn('', '');
X = zeros(3, 2) \ [1; 1; 1];
[msg, id] = lastwarn();
asserts.isequal(X, [0; 0]);
asserts.isequal(id, 'Nelson:rankDeficientMatrix');
asserts.isequal(msg, sprintf(_('Matrix is rank deficient, rank = %d, tol = %e.'), 0, 0));
asserts.isequal(zeros(2, 3) \ [1; 1], [0; 0; 0]);
%=============================================================================
% Single precision.
lastwarn('', '');
X = single([1 2; 2 4; 3 6]) \ single([1; 1; 1]);
[msg, id] = lastwarn();
asserts.isequal(class(X), 'single');
asserts.isapprox(X, single([0; 0.21428570151329041]), single(1e-6));
asserts.isequal(X(1), single(0));
asserts.isequal(msg, sprintf(_('Matrix is rank deficient, rank = %d, tol = %e.'), 1, 2.676242e-06));
%=============================================================================
% Complex data uses a ten times larger rank tolerance.
lastwarn('', '');
X = [1 2i; 2 4i; 3 6i] \ [1; 1; 1];
[msg, id] = lastwarn();
% the zero real part may keep a rounding residue on some CPUs, hence the absolute tolerance
asserts.isapprox(X, [0; -0.21428571428571433i], 1e-15, 1e-15);
asserts.isequal(msg, sprintf(_('Matrix is rank deficient, rank = %d, tol = %e.'), 1, 4.984889e-14));
lastwarn('', '');
X = single([1 2i; 2 4i; 3 6i]) \ single([1; 1; 1]);
[msg, id] = lastwarn();
asserts.isequal(class(X), 'single');
asserts.isapprox(imag(X), single([0; -0.21428570151329041]), single(1e-6));
asserts.isequal(msg, sprintf(_('Matrix is rank deficient, rank = %d, tol = %e.'), 1, 2.676242e-05));
X = [1 2i 3; 2 4i 6] \ [1; 2];
asserts.isapprox(X, [0; 0; 1/3], 1e-15);
%=============================================================================
% Full rank least squares and NaN data do not warn.
lastwarn('', '');
X = [1 2; 3 4; 5 7] \ [1; 2; 3];
[msg, id] = lastwarn();
asserts.isequal(id, '');
asserts.isapprox(X, [-0.071428571428572424; 0.50000000000000078], 1e-13);
X = [1 NaN; 2 3; 4 5] \ [1; 1; 1];
[msg, id] = lastwarn();
asserts.isequal(id, '');
asserts.istrue(isequaln(X, [NaN; NaN]));
%=============================================================================
