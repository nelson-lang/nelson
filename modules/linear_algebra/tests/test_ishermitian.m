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
asserts.isequal(nargin('ishermitian'), 2);
asserts.isequal(nargout('ishermitian'), 1);
%=============================================================================
asserts.istrue(ishermitian([0, 2i; 2i, 0], 'skew'));
asserts.isfalse(ishermitian([0, 2i; 2i, 0], 'nonskew'));
%=============================================================================
asserts.isfalse(ishermitian([1 0 1i; 0 1 0; 1i 0 1]));
asserts.istrue(ishermitian([1 0 1i; 0 1 0; -1i 0 1]));
%=============================================================================
asserts.istrue(ishermitian([-1i -1 1 - i; 1 -1i -1; -1 - i 1 -1i], 'skew'));
asserts.isfalse(ishermitian([-1i -1 1 - i; 1 -1i -1; -1 - i 1 -1i], 'nonskew'));
%=============================================================================
asserts.istrue(ishermitian(1));
asserts.isfalse(ishermitian([1, 2]));
asserts.istrue(ishermitian([]));
asserts.istrue(ishermitian(NaN));
asserts.istrue(ishermitian(Inf));
asserts.istrue(ishermitian([1, 2; 2, 1]));
asserts.isfalse(ishermitian([1, 2i; 2i, 1]));
%=============================================================================
A = [1 0 1i; 0 1 0; -1i 0 1];
asserts.istrue(ishermitian(A));
asserts.isfalse(ishermitian(A, 'skew'));
asserts.istrue(ishermitian(A, 'nonskew'));
%=============================================================================
A = [0 1 -2 5; -1 0 3 -4; 2 -3 0 6; -5 4 -6 0];
asserts.istrue(ishermitian(A, 'skew'));
asserts.isfalse(ishermitian(A, 'nonskew'));
asserts.isfalse(ishermitian(A));
%=============================================================================
A = [0 1 -2 5; -1 0 3 -4; 2 -3 0 6; -5 4 -6 0] * i;
asserts.isfalse(ishermitian(A, 'skew'));
asserts.istrue(ishermitian(A, 'nonskew'));
asserts.istrue(ishermitian(A));
%=============================================================================
A = [0 NaN(); NaN() 0];
asserts.isfalse(ishermitian(A, 'skew'));
asserts.isfalse(ishermitian(A, 'nonskew'));
asserts.isfalse(ishermitian(A));
%=============================================================================
A = [0 1; 1 0];
asserts.isfalse(ishermitian(A, 'skew'));
asserts.istrue(ishermitian(A, 'nonskew'));
asserts.istrue(ishermitian(A));
%=============================================================================
A = single([0 1; 1 0]);
asserts.isfalse(ishermitian(A, 'skew'));
asserts.istrue(ishermitian(A, 'nonskew'));
asserts.istrue(ishermitian(A));
%=============================================================================
A = int8([0 1; 1 0]);
asserts.isfalse(ishermitian(A, 'skew'));
asserts.istrue(ishermitian(A, 'nonskew'));
asserts.istrue(ishermitian(A));
%=============================================================================
A = uint8([0 1; 1 0]);
asserts.isfalse(ishermitian(A, 'skew'));
asserts.istrue(ishermitian(A, 'nonskew'));
asserts.istrue(ishermitian(A));
%=============================================================================
asserts.isfalse(ishermitian(ones(3, 3, 3)));
%=============================================================================
asserts.isfalse(ishermitian(-2i - 9))
asserts.isfalse(ishermitian(2i - 9))
asserts.isfalse(ishermitian(-2i))
asserts.isfalse(ishermitian(2i))
asserts.istrue(ishermitian(3))
%=============================================================================
asserts.istrue(ishermitian([1 1i; -1i 1]));
asserts.istrue(ishermitian([0, 2i; 2i, 0], 'skew'));
%=============================================================================
