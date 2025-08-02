%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('issymmetric'), 2);
assert_isequal(nargout('issymmetric'), 1);
%=============================================================================
assert_istrue(issymmetric(1));
assert_isfalse(issymmetric([1, 2]));
assert_istrue(issymmetric([]));
assert_istrue(issymmetric(NaN));
assert_istrue(issymmetric(Inf));
assert_istrue(issymmetric([1, 2; 2, 1]));
assert_istrue(issymmetric([1, 2.1; 2, 1.1], 0.2));
assert_istrue(issymmetric([1, 2i; 2i, 1]));
%=============================================================================
A = [1 0 1i; 0 1 0;-1i 0 1];
assert_isfalse(issymmetric(A));
assert_isfalse(issymmetric(A, 'skew'));
assert_isfalse(issymmetric(A, 'nonskew'));
%=============================================================================
A = [1 0 1i; 0 1 0;-1i 0 1];
A(3,1) = 1i;
assert_isfalse(issymmetric(A, 'skew'));
assert_istrue(issymmetric(A, 'nonskew'));
assert_istrue(issymmetric(A));
%=============================================================================
A = [0 1 -2 5; -1 0 3 -4; 2 -3 0 6; -5 4 -6 0];
assert_istrue(issymmetric(A, 'skew'));
assert_isfalse(issymmetric(A, 'nonskew'));
assert_isfalse(issymmetric(A));
%=============================================================================
A = [0 1 -2 5; -1 0 3 -4; 2 -3 0 6; -5 4 -6 0] * i;
assert_istrue(issymmetric(A, 'skew'));
assert_isfalse(issymmetric(A, 'nonskew'));
assert_isfalse(issymmetric(A));
%=============================================================================
A = [0 NaN(); NaN() 0];
assert_isfalse(issymmetric(A, 'skew'));
assert_isfalse(issymmetric(A, 'nonskew'));
assert_isfalse(issymmetric(A));
%=============================================================================
A = [0 1; 1 0];
assert_isfalse(issymmetric(A, 'skew'));
assert_istrue(issymmetric(A, 'nonskew'));
assert_istrue(issymmetric(A));
%=============================================================================
A = single([0 1; 1 0]);
assert_isfalse(issymmetric(A, 'skew'));
assert_istrue(issymmetric(A, 'nonskew'));
assert_istrue(issymmetric(A));
%=============================================================================
A = int8([0 1; 1 0]);
assert_isfalse(issymmetric(A, 'skew'));
assert_istrue(issymmetric(A, 'nonskew'));
assert_istrue(issymmetric(A));
%=============================================================================
A = uint8([0 1; 1 0]);
assert_isfalse(issymmetric(A, 'skew'));
assert_istrue(issymmetric(A, 'nonskew'));
assert_istrue(issymmetric(A));
%=============================================================================
A = logical([0 1; 1 0]);
assert_isfalse(issymmetric(A, 'skew'));
assert_istrue(issymmetric(A, 'nonskew'));
assert_istrue(issymmetric(A));
%=============================================================================
assert_isfalse(issymmetric(ones(3,3,3)));
%=============================================================================
assert_istrue(issymmetric(-2i-9))
assert_istrue(issymmetric(2i-9))
assert_istrue(issymmetric(-2i))
assert_istrue(issymmetric(2i))
assert_istrue(issymmetric(3))
%=============================================================================
