%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% A' \ B factors A itself: same RCOND and bits as (B' / A)'.
A = single([3+4i 1-2i 2; 1+1i 2i 3-1i; 4+5i 1 5-1i+2e-15]);
b = single([1; 2; 3]);
lastwarn('', '');
x = A' \ b;
[msg, id] = lastwarn();
asserts.isequal(id, 'Nelson:nearlySingularMatrix');
asserts.istrue(contains(msg, '3.178369e-09'));
asserts.isequal(x, (b' / A)');
lastwarn('', '');
x = (A') \ [b, 2 * b];
msg = lastwarn();
asserts.istrue(contains(msg, '3.178369e-09'));
asserts.isequal(size(x), [3 2]);
%=============================================================================
% a stored transpose is factored as given
At = A';
lastwarn('', '');
x = At \ b;
[msg, id] = lastwarn();
asserts.isequal(id, 'Nelson:singularMatrix');
%=============================================================================
% A.' \ b of a complex A forms the transpose first
lastwarn('', '');
x = A.' \ b;
[msg, id] = lastwarn();
asserts.isequal(id, 'Nelson:singularMatrix');
%=============================================================================
% real A: ' and .' give the same solve
R = [1 2 3; 4 5 6; 7 8 9];
r = [1; 2; 3];
lastwarn('', '');
x = R' \ r;
[msg, id] = lastwarn();
asserts.isequal(id, 'Nelson:nearlySingularMatrix');
asserts.istrue(contains(msg, '1.156482e-18'));
lastwarn('', '');
y = R.' \ r;
msg = lastwarn();
asserts.istrue(contains(msg, '1.156482e-18'));
asserts.isequal(x, y);
Rt = R';
lastwarn('', '');
x = Rt \ r;
[msg, id] = lastwarn();
asserts.isequal(id, 'Nelson:singularMatrix');
%=============================================================================
% well conditioned, triangular, diagonal, Hermitian and rectangular operands
rng(11);
M = rand(6) + 6 * eye(6);
c = rand(6, 1);
asserts.isapprox(M' \ c, M.' \ c);
asserts.istrue(norm(M' * (M' \ c) - c, 1) < 1e-12);
U = triu(M);
asserts.isequal(U' \ c, (c' / U)');
D = diag([2 4 8]);
asserts.isequal(D' \ [1; 1; 1], [0.5; 0.25; 0.125]);
H = M * M';
asserts.isequal(H' \ c, (c' / H)');
Q = rand(5, 3);
q = rand(3, 1);
Qt = Q';
asserts.isequal(Q' \ q, Qt \ q);
%=============================================================================
% other classes keep the transpose then divide route
S = sparse(M);
asserts.isapprox(full(S' \ c), M' \ c, 1e-12);
I = int8([2 0; 0 4]);
It = I';
try
  expected = It \ int8([4; 8]);
catch e
  expected = e.message;
end
try
  x = I' \ int8([4; 8]);
catch e
  x = e.message;
end
asserts.isequal(x, expected);
asserts.isequal(5' \ 10, 2);
z = complex(M, M);
asserts.isapprox(z' \ c, (c' / z)');
asserts.isequal(class(single(M)' \ c), 'single');
%=============================================================================
