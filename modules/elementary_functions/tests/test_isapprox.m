%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 4-->
asserts.checkerror('isapprox()', _('Not enough input arguments.'));
asserts.checkerror('isapprox(1)', _('Not enough input arguments.'));
asserts.checkerror('isapprox([1, 1])', _('Not enough input arguments.'));
%=============================================================================
asserts.isequal(nargin('isapprox'), -2);
asserts.isequal(nargout('isapprox'), 1);
%=============================================================================
asserts.istrue(isapprox(1, 1));
%=============================================================================
A = pi;
B = single(pi);
asserts.isfalse(isapprox(A, B));
asserts.istrue(isapprox(A, B, 1e-4));
%=============================================================================
asserts.isfalse(isapprox(1, [2 3], eps));
asserts.isfalse(isapprox(zeros(10, 1) + 1.e-4, zeros(10, 1), 1.e-5));
asserts.istrue(isapprox([], []));
%=============================================================================
asserts.istrue(isapprox(ones(10, 1), ones(10, 1), eps));
asserts.istrue(isapprox(ones(10, 1) + eps, ones(10, 1), eps));
asserts.istrue(isapprox(zeros(10, 1), zeros(10, 1), 1.e-5));
%=============================================================================
asserts.isfalse(isapprox(1, 2, eps));
asserts.isfalse(isapprox(zeros(10, 1) + 1.e-4, zeros(10, 1), 1.e-5));
%=============================================================================
asserts.istrue(isapprox(1.23456789123456789e-30, 1.23456789123456789e-30, eps));
%=============================================================================
asserts.isfalse(isapprox(1.23456789123456789e-30, 1.3e-30, eps));
%=============================================================================
asserts.istrue(isapprox([1 NaN], [1 NaN], eps));
asserts.istrue(isapprox([1 NaN 1 Inf], [1 NaN 1 Inf], eps));
%=============================================================================
asserts.isfalse(isapprox([NaN 1], [1 NaN], eps));
asserts.isfalse(isapprox(1 + 5 * eps, 1, eps));
asserts.istrue(isapprox(1 + 5 * eps, 1, 10 * eps));
asserts.istrue(isapprox(1.23456, 1.23457, 1.e11 * eps));
%=============================================================================
asserts.istrue(isapprox([1.2345 Inf -Inf NaN], [1.2346 Inf -Inf NaN], 1.e-4));
%=============================================================================
asserts.istrue(isapprox([1 1.e5], [2 1.e5], 1.e-3));
asserts.istrue(isapprox([1 1.e5], [1 1.e5], 1.e-3));
%=============================================================================
asserts.isfalse(isapprox([1 NaN], [2 NaN], 1.e-3));
asserts.isfalse(isapprox([1 Inf], [2 Inf], 1.e-3));
%=============================================================================
asserts.istrue(isapprox([1 Inf], [1 Inf], 1.e-3));
%=============================================================================
asserts.istrue(isapprox([1 Inf -Inf NaN], [1 Inf -Inf NaN], 1.e-3));
asserts.isfalse(isapprox([1 Inf -Inf NaN], [1 -Inf Inf NaN], 1.e-3));
%=============================================================================
asserts.istrue(isapprox(1 + i, 1 + (1 + 1.e-4) * i, 1.e-3));
asserts.isfalse(isapprox(1 + i, 1 + (1 + 1.e-4) * i, 1.e-5));
%=============================================================================
A = sparse(eye(3, 3) + eps);
B = sparse(eye(3, 3) + 2 * eps);
asserts.istrue(isapprox(A, B, 10 * eps));
%=============================================================================
asserts.istrue(isapprox(int8(3), 3));
asserts.istrue(isapprox(int8(3), int16(3)));
%=============================================================================
A = rand(3, 3, 3);
asserts.istrue(isapprox(A, A));
%=============================================================================
asserts.checkerror('isapprox(1,''A'')', _('Numerics types expected.'));
%=============================================================================
