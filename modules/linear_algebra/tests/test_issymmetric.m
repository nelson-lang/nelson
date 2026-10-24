%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
% <--REUSE PROCESS-->
asserts.isequal(nargin('issymmetric'), 2);
asserts.isequal(nargout('issymmetric'), 1);
%=============================================================================
asserts.istrue(issymmetric(1));
asserts.isfalse(issymmetric([1, 2]));
asserts.istrue(issymmetric([]));
asserts.istrue(issymmetric(NaN));
asserts.istrue(issymmetric(Inf));
asserts.istrue(issymmetric([1, 2; 2, 1]));
asserts.istrue(issymmetric([1, 2.1; 2, 1.1], 0.2));
asserts.istrue(issymmetric([1, 2i; 2i, 1]));
%=============================================================================
A = [1 0 1i; 0 1 0; -1i 0 1];
asserts.isfalse(issymmetric(A));
asserts.isfalse(issymmetric(A, 'skew'));
asserts.isfalse(issymmetric(A, 'nonskew'));
%=============================================================================
A = [1 0 1i; 0 1 0; -1i 0 1];
A(3, 1) = 1i;
asserts.isfalse(issymmetric(A, 'skew'));
asserts.istrue(issymmetric(A, 'nonskew'));
asserts.istrue(issymmetric(A));
%=============================================================================
A = [0 1 -2 5; -1 0 3 -4; 2 -3 0 6; -5 4 -6 0];
asserts.istrue(issymmetric(A, 'skew'));
asserts.isfalse(issymmetric(A, 'nonskew'));
asserts.isfalse(issymmetric(A));
%=============================================================================
A = [0 1 -2 5; -1 0 3 -4; 2 -3 0 6; -5 4 -6 0] * i;
asserts.istrue(issymmetric(A, 'skew'));
asserts.isfalse(issymmetric(A, 'nonskew'));
asserts.isfalse(issymmetric(A));
%=============================================================================
A = [0 NaN(); NaN() 0];
asserts.isfalse(issymmetric(A, 'skew'));
asserts.isfalse(issymmetric(A, 'nonskew'));
asserts.isfalse(issymmetric(A));
%=============================================================================
A = [0 1; 1 0];
asserts.isfalse(issymmetric(A, 'skew'));
asserts.istrue(issymmetric(A, 'nonskew'));
asserts.istrue(issymmetric(A));
%=============================================================================
A = single([0 1; 1 0]);
asserts.isfalse(issymmetric(A, 'skew'));
asserts.istrue(issymmetric(A, 'nonskew'));
asserts.istrue(issymmetric(A));
%=============================================================================
A = int8([0 1; 1 0]);
asserts.isfalse(issymmetric(A, 'skew'));
asserts.istrue(issymmetric(A, 'nonskew'));
asserts.istrue(issymmetric(A));
%=============================================================================
A = uint8([0 1; 1 0]);
asserts.isfalse(issymmetric(A, 'skew'));
asserts.istrue(issymmetric(A, 'nonskew'));
asserts.istrue(issymmetric(A));
%=============================================================================
A = logical([0 1; 1 0]);
asserts.isfalse(issymmetric(A, 'skew'));
asserts.istrue(issymmetric(A, 'nonskew'));
asserts.istrue(issymmetric(A));
%=============================================================================
asserts.isfalse(issymmetric(ones(3, 3, 3)));
%=============================================================================
asserts.istrue(issymmetric(-2i - 9))
asserts.istrue(issymmetric(2i - 9))
asserts.istrue(issymmetric(-2i))
asserts.istrue(issymmetric(2i))
asserts.istrue(issymmetric(3))
%=============================================================================
singleInput = single([2 0; 0 4]);
asserts.isequal(issymmetric(singleInput), true);
asserts.istrue(issymmetric([1, 2.1; 2, 1.1], 0.2));
asserts.istrue(issymmetric([0 1; -1 0], 'skew'));
%=============================================================================
