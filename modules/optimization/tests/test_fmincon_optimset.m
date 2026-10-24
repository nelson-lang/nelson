%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% fmincon must accept a legacy optimset() options structure and resolve every
% unset field to its concrete default. optimset leaves
% unset options empty ([]); fmincon fills them from its own defaults.
%=============================================================================
% Unconstrained-style objective with a known minimum at [1; 2].
obj = @(x)(x(1) - 1) ^ 2 + (x(2) - 2) ^ 2;
x0 = [0; 0];
expected = [1; 2];
%=============================================================================
% optimset leaves unset options empty, so fmincon can default them.
o = optimset('Display', 'off');
asserts.istrue(isempty(o.Algorithm));
asserts.istrue(isempty(o.TolX));
asserts.istrue(isempty(o.ConstraintTolerance));
%=============================================================================
% (a) no options structure at all.
x = fmincon(obj, x0, [], [], [], [], [], []);
asserts.isapprox(x(:), expected, 1e-4);
%=============================================================================
% (b) optimset('Display', 'off'): Algorithm unset -> interior-point default.
o = optimset('Display', 'off');
[x, fval, exitflag] = fmincon(obj, x0, [], [], [], [], [], [], [], o);
asserts.isapprox(x(:), expected, 1e-4);
asserts.istrue(exitflag > 0);
%=============================================================================
% (c) optimset with Algorithm 'sqp' (the empty-tolerance && path).
o = optimset('Display', 'off', 'Algorithm', 'sqp');
[x, fval, exitflag] = fmincon(obj, x0, [], [], [], [], [], [], [], o);
asserts.isapprox(x(:), expected, 1e-4);
asserts.istrue(exitflag > 0);
%=============================================================================
% (d) optimset with Algorithm 'interior-point'.
o = optimset('Display', 'off', 'Algorithm', 'interior-point');
[x, fval, exitflag] = fmincon(obj, x0, [], [], [], [], [], [], [], o);
asserts.isapprox(x(:), expected, 1e-4);
asserts.istrue(exitflag > 0);
%=============================================================================
% (e) legacy option names (MaxIter/TolCon).
o = optimset('MaxIter', 50, 'TolCon', 1e-8, 'Display', 'off');
[x, fval, exitflag] = fmincon(obj, x0, [], [], [], [], [], [], [], o);
asserts.isapprox(x(:), expected, 1e-4);
asserts.istrue(exitflag > 0);
%=============================================================================
% Constrained QP with an optimset structure, both default and sqp algorithms.
% min (x1-1)^2 + (x2-2)^2  s.t.  x1 + x2 <= 2  ->  x* = [0.5; 1.5], fval = 0.5.
A = [1, 1];
b = 2;
qpExpected = [0.5; 1.5];
o = optimset('Display', 'off');
[x, fval] = fmincon(obj, x0, A, b, [], [], [], [], [], o);
asserts.isapprox(x(:), qpExpected, 1e-3);
asserts.isapprox(fval, 0.5, 1e-3);
o = optimset('Display', 'off', 'Algorithm', 'sqp');
[x, fval] = fmincon(obj, x0, A, b, [], [], [], [], [], o);
asserts.isapprox(x(:), qpExpected, 1e-3);
asserts.isapprox(fval, 0.5, 1e-3);
%=============================================================================
