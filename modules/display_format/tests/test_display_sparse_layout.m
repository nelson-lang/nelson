%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% sparse display: header with the number of nonzeros, "(row,column)" aligned
% on the comma, placed by the size of the matrix
%=============================================================================
format('default');
format('compact');
R = evalc('a = sparse([1 0; 0 2])');
REF = sprintf('a =\n  2×2 sparse double matrix (2 nonzeros)\n   (1,1)        1\n   (2,2)        2\n');
asserts.isequal(R, REF);
%=============================================================================
R = evalc('b = sparse([1.5 0; 0 -2])');
REF = sprintf('b =\n  2×2 sparse double matrix (2 nonzeros)\n   (1,1)       1.5000\n   (2,2)      -2.0000\n');
asserts.isequal(R, REF);
%=============================================================================
R = evalc('c = sparse(logical([1 0; 0 1]))');
REF = sprintf('c =\n  2×2 sparse logical array (2 nonzeros)\n   (1,1)   true\n   (2,2)   true\n');
asserts.isequal(R, REF);
%=============================================================================
R = evalc('c = sparse([1234.5+1i 0; 0 1])');
REF = sprintf('c =\n  2×2 sparse complex double matrix (2 nonzeros)\n   1.0e+03 *\n   (1,1)      1.2345 + 0.0010i\n   (2,2)      0.0010 + 0.0000i\n');
asserts.isequal(R, REF);
%=============================================================================
R = evalc('d = sparse(eye(10))');
REF = sprintf(['d =\n  10×10 sparse double matrix (10 nonzeros)\n', ...
  '   (1,1)        1\n   (2,2)        1\n   (3,3)        1\n   (4,4)        1\n', ...
  '   (5,5)        1\n   (6,6)        1\n   (7,7)        1\n   (8,8)        1\n', ...
  '   (9,9)        1\n  (10,10)       1\n']);
asserts.isequal(R, REF);
%=============================================================================
R = evalc('e = sparse([0 0; 0 0])');
REF = sprintf('e =\n  2×2 sparse double matrix\n   All zero\n');
asserts.isequal(R, REF);
%=============================================================================
R = evalc('h = sparse(3)');
REF = sprintf('h =\n  sparse double\n   (1,1)        3\n');
asserts.isequal(R, REF);
%=============================================================================
R = evalc('r = sparse(1, 1, 7, 100, 1000)');
REF = sprintf('r =\n  100×1000 sparse double matrix (1 nonzero)\n   (1,1)        7\n');
asserts.isequal(R, REF);
%=============================================================================
R = evalc('v = sparse([1 2 0 5])');
REF = sprintf('v =\n  1×4 sparse double row vector (3 nonzeros)\n   (1,1)        1\n   (1,2)        2\n   (1,4)        5\n');
asserts.isequal(R, REF);
%=============================================================================
R = evalc('p = sparse(zeros(0, 3))');
REF = sprintf('p =\n  0×3 empty sparse double matrix\n');
asserts.isequal(R, REF);
%=============================================================================
% the comma column depends on the largest dimension
R = evalc('disp(sparse([5 1000], [1000 5], [1 2], 1000, 1000))');
REF = sprintf('(1000,5)        2\n   (5,1000)     1\n');
asserts.isequal(R, REF);
R = evalc('disp(sparse([1 10000], [1 1], [1 2], 10000, 1))');
REF = sprintf('         (1,1)              1\n     (10000,1)              2\n');
asserts.isequal(R, REF);
R = evalc('disp(sparse([1 1], [1 100000], [1 2], 1, 100000))');
REF = sprintf('                  (1,1)                       1\n                  (1,100000)                  2\n');
asserts.isequal(R, REF);
%=============================================================================
format('loose');
R = evalc('b = sparse([1e5+0.5 0; 0 2])');
REF = sprintf('\nb =\n\n  2×2 sparse double matrix (2 nonzeros)\n\n   1.0e+05 *\n\n   (1,1)       1.0000\n   (2,2)       0.0000\n\n');
asserts.isequal(R, REF);
R = evalc('disp(sparse([1 0; 0 2]))');
REF = sprintf('   (1,1)        1\n   (2,2)        2\n\n');
asserts.isequal(R, REF);
R = evalc('disp(sparse([0 0]))');
REF = sprintf('   All zero\n\n');
asserts.isequal(R, REF);
%=============================================================================
format('default');
