%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% distinct poles
[r, p, k] = residue([1 0], [1 -3 2]);
asserts.isapprox(r, [2; -1], 1e-12);
asserts.isapprox(p, [2; 1], 1e-12);
asserts.isequal(k, []);
%=============================================================================
% repeated poles: (s^2 + 2 s + 3) / (s + 1)^3
[r, p, k] = residue([1 2 3], [1 3 3 1]);
asserts.isapprox(r(1), 1, 1e-9);
asserts.istrue(abs(r(2)) < 1e-6);
asserts.isapprox(r(3), 2, 1e-9);
asserts.isapprox(p, [-1; -1; -1], 1e-6);
asserts.isequal(k, []);
%=============================================================================
% improper rational (numerator degree equal to denominator degree)
[r, p, k] = residue([2 5 3 6], [1 6 11 6]);
asserts.isapprox(r, [-6; -4; 3], 1e-9);
asserts.isapprox(p, [-3; -2; -1], 1e-12);
asserts.isapprox(k, 2, 1e-12);
%=============================================================================
% reverse form recovers the original polynomials (distinct case)
[r, p, k] = residue([1 0], [1 -3 2]);
[b, a] = residue(r, p, k);
asserts.isapprox(b, [1 0], 1e-12);
asserts.isapprox(a, [1 -3 2], 1e-12);
%=============================================================================
% reverse form recovers the original polynomials (improper case)
[r, p, k] = residue([2 5 3 6], [1 6 11 6]);
[b, a] = residue(r, p, k);
asserts.isapprox(b, [2 5 3 6], 1e-9);
asserts.isapprox(a, [1 6 11 6], 1e-9);
%=============================================================================
% mixed multiplicity: (s + 1) / (s (s + 2)^2)
[r, p, k] = residue([1 1], poly([0 -2 -2]));
asserts.isapprox(r, [-0.25; 0.5; 0.25], 1e-9);
asserts.isapprox(p, [-2; -2; 0], 1e-9);
asserts.isequal(k, []);
%=============================================================================
% reverse form of the mixed case
[b, a] = residue(r, p, k);
asserts.isapprox(b, [0 1 1], 1e-9);
asserts.isapprox(a, [1 4 4 0], 1e-9);
%=============================================================================
% complex conjugate poles: 1 / (s^2 + 1)
[r, p, k] = residue(1, [1 0 1]);
asserts.isapprox(r, [0 - 0.5i; 0 + 0.5i], 1e-9);
asserts.isapprox(p, [0 + 1i; 0 - 1i], 1e-9);
asserts.isequal(k, []);
%=============================================================================
% direct term with more than one coefficient
[r, p, k] = residue([1 0 0], [1 1]);
asserts.isapprox(r, 1, 1e-12);
asserts.isapprox(p, -1, 1e-12);
asserts.isapprox(k, [1 -1], 1e-12);
%=============================================================================
% two output forward form
[r, p] = residue([1 0], [1 -3 2]);
asserts.isapprox(r, [2; -1], 1e-12);
asserts.isapprox(p, [2; 1], 1e-12);
%=============================================================================
asserts.checkerror('residue(1)', _('Not enough input arguments.'));
%=============================================================================
