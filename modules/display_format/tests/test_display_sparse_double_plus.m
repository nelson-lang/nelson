%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ENGLISH IMPOSED-->
%=============================================================================
format('+')
%=============================================================================
R = evalc('A = sparse([])');
REF = '
A =

  0×0 empty sparse double matrix

';
asserts.isequal(R, REF)
%=============================================================================
R = evalc('A = sparse(ones(0,4))');
REF = '
A =

  0×4 empty sparse double matrix

';
asserts.isequal(R, REF)
%=============================================================================
R = evalc('A = sparse(10000,5000)');
REF = '
A =

  10000×5000 sparse double matrix

   All zero

';
asserts.isequal(R, REF)
%=============================================================================
R = evalc('A = sparse(eps)');
REF = '
A =

  sparse double

   (1,1)   +

';
asserts.isequal(R, REF)
%=============================================================================
R = evalc('A = sparse(NaN)');
REF = '
A =

  sparse double

   (1,1)   +

';
asserts.isequal(R, REF)
%=============================================================================
R = evalc('A = sparse(Inf)');
REF = '
A =

  sparse double

   (1,1)   +

';
asserts.isequal(R, REF)
%=============================================================================
R = evalc('A = sparse(-Inf)');
REF = '
A =

  sparse double

   (1,1)   -

';
asserts.isequal(R, REF)
%=============================================================================
R = evalc('A = sparse([1e-3 -1.1e-3 1.2e-3])');
REF = '
A =

  1×3 sparse double row vector (3 nonzeros)

   (1,1)   +
   (1,2)   -
   (1,3)   +

';
asserts.isequal(R, REF)
%=============================================================================
R = evalc('A = sparse([10 -10 13])');
REF = '
A =

  1×3 sparse double row vector (3 nonzeros)

   (1,1)   +
   (1,2)   -
   (1,3)   +

';
asserts.isequal(R, REF)
%=============================================================================
A = zeros(4000, 4000);
A(3, 3) = 1e-3;
A(1000, 1000) = 1;
R = evalc('R = sparse(A)');
REF = '
R =

  4000×4000 sparse double matrix (2 nonzeros)

   (3,3)   +
(1000,1000)+

';
asserts.isequal(R, REF)
%=============================================================================
