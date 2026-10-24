%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_checkerror('assert_isapprox()', _('Not enough input arguments.'));
assert_checkerror('assert_isapprox(1)', _('Not enough input arguments.'));
assert_checkerror('assert_isapprox([1, 1])', _('Not enough input arguments.'));
assert_isequal(nargin('assert_isapprox'), -2);
assert_isequal(nargout('assert_isapprox'), 2);
assert_isequal(nargin('asserts.isapprox'), -2);
assert_isequal(nargout('asserts.isapprox'), 2);
%=============================================================================
precision_msg = _('Precision must be a nonnegative finite numeric scalar.');
assert_checkerror('assert_isapprox(1, 1, NaN)', precision_msg);
assert_checkerror('assert_isapprox(1, 1, NaN)', precision_msg, 'Nelson:asserts:invalidTolerance');
assert_checkerror('assert_isapprox(1, 1, Inf)', precision_msg);
assert_checkerror('assert_isapprox(1, 1, -1)', precision_msg);
assert_checkerror('assert_isapprox(1, 1, [1 2])', precision_msg);
absolute_tolerance_msg = _('Absolute tolerance must be a nonnegative finite numeric scalar.');
assert_checkerror('assert_isapprox(1, 1, 0, NaN)', absolute_tolerance_msg);
assert_checkerror('assert_isapprox(1, 1, 0, Inf)', absolute_tolerance_msg);
assert_checkerror('assert_isapprox(1, 1, 0, -1)', absolute_tolerance_msg);
assert_checkerror('assert_isapprox(1, 1, 0, [1 2])', absolute_tolerance_msg);
assert_checkerror('assert_isapprox(1, 1, 0, struct())', absolute_tolerance_msg);
%=============================================================================
assert_isapprox(1, 1);
asserts.isapprox(1, 1);
%=============================================================================
A = pi;
B = single(pi);
r = assert_isapprox(A, B);
assert_isfalse(r);
%=============================================================================
assert_isapprox(A, B, 1e-4);
%=============================================================================
r = assert_isapprox(1, [2 3], eps);
assert_isfalse(r);
%=============================================================================
[r, msg] = assert_isapprox([1 2], [1 3], eps);
assert_isfalse(r);
assert_istrue(contains(msg, 'Expected: class'));
assert_istrue(contains(msg, 'Computed: class'));
assert_istrue(contains(msg, 'Relative tolerance:'));
assert_istrue(contains(msg, 'Absolute tolerance:'));
assert_istrue(contains(msg, 'First different index: (1, 2)'));
%=============================================================================
[r, msg] = assert_isapprox(1, 2, 'custom failure');
assert_isfalse(r);
assert_istrue(contains(msg, 'custom failure'));
%=============================================================================
[r, msg] = asserts.isapprox(1, 2, eps, 'custom failure');
assert_isfalse(r);
assert_istrue(contains(msg, 'custom failure'));
%=============================================================================
assert_isapprox(1, 1 + 1e-8, 0, 1e-7);
asserts.isapprox(1, 1 + 1e-8, 0, 1e-7);
[r, msg] = assert_isapprox(1, 1 + 1e-8, 0, 1e-10);
assert_isfalse(r);
assert_istrue(contains(msg, 'Absolute tolerance:'));
%=============================================================================
r = assert_isapprox(zeros(10, 1) + 1.e-4, zeros(10, 1), 1.e-5);
assert_isfalse(r);
%=============================================================================
assert_isapprox([], []);
assert_isapprox(ones(10, 1), ones(10, 1), eps);
assert_isapprox(ones(10, 1) + eps, ones(10, 1), eps);
assert_isapprox(zeros(10, 1), zeros(10, 1), 1.e-5);
%=============================================================================
r = assert_isapprox(1, 2, eps);
assert_isfalse(r);
%=============================================================================
r = assert_isapprox(zeros(10, 1) + 1.e-4, zeros(10, 1), 1.e-5);
assert_isfalse(r);
%=============================================================================
assert_isapprox(1.23456789123456789e-30, 1.23456789123456789e-30, eps);
%=============================================================================
r = assert_isapprox(9e-30, 1.3e-30, eps);
assert_isfalse(r);
%=============================================================================
assert_isapprox([1 NaN], [1 NaN], eps);
assert_isapprox([1 NaN 1 Inf], [1 NaN 1 Inf], eps);
%=============================================================================
r = assert_isapprox([NaN 1], [1 NaN], eps);
assert_isfalse(r);
%=============================================================================
r = assert_isapprox(1 + 5 * eps, 1, eps);
assert_isfalse(r);
%=============================================================================
assert_isapprox(1 + 5 * eps, 1, 10 * eps);
assert_isapprox(1.23456, 1.23457, 1.e11 * eps);
%=============================================================================
assert_isapprox([1.2345 Inf -Inf NaN], [1.2346 Inf -Inf NaN], 1.e-4);
%=============================================================================
assert_isapprox([1 1.e5], [2 1.e5], 1.e-3);
assert_isapprox([1 1.e5], [1 1.e5], 1.e-3);
%=============================================================================
r = assert_isapprox([1 NaN], [2 NaN], 1.e-3);
assert_isfalse(r);
%=============================================================================
r = assert_isapprox([1 Inf], [2 Inf], 1.e-3);
assert_isfalse(r);
%=============================================================================
assert_isapprox([1 Inf], [1 Inf], 1.e-3);
%=============================================================================
assert_isapprox([1 Inf -Inf NaN], [1 Inf -Inf NaN], 1.e-3);
r = assert_isapprox([1 Inf -Inf NaN], [1 -Inf Inf NaN], 1.e-3);
assert_isfalse(r);
%=============================================================================
assert_isapprox(1 + i, 1 + (1 + 1.e-4) * i, 1.e-3);
r = assert_isapprox(1 + i, 1 + (1 + 1.e-4) * i, 1.e-5);
assert_isfalse(r);
%=============================================================================
A = sparse(eye(3, 3) + eps);
B = sparse(eye(3, 3) + 2 * eps);
assert_isapprox(A, B, 10 * eps);
%=============================================================================
assert_isapprox(int8(3), 3);
assert_isapprox(int8(3), int16(3));
%=============================================================================
A = rand(3, 3, 3);
assert_isapprox(A, A);
%=============================================================================

