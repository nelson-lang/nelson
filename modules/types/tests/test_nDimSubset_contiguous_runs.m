%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 4-->
% A(a:b, ...) copies one contiguous block per column; the reference values are
% taken with a descending subscript, which runs the element-wise path.
%=============================================================================
A = reshape(1:20, [5 4]);
asserts.isequal(A(2:4, :), [2 7 12 17; 3 8 13 18; 4 9 14 19]);
asserts.isequal(A(2:4, 3), [12; 13; 14]);
asserts.isequal(A(2:4, [4 1]), [17 2; 18 3; 19 4]);
asserts.isequal(A(2:4, [2 2]), [7 7; 8 8; 9 9]);
asserts.isequal(A(3:end, end), [18; 19; 20]);
asserts.isequal(A(end - 1:end, 2:3), [9 14; 10 15]);
asserts.isequal(A(1:end, 2), (6:10)');
asserts.isequal(A(3:3, :), [3 8 13 18]);
asserts.isequal(A(3, 2:4), [8 13 18]);
asserts.isequal(A(:, 2:3), reshape(6:15, [5 2]));
%=============================================================================
M = rand(1000, 7);
asserts.isequal(M(101:900, :), flipud(M(900:-1:101, :)));
asserts.isequal(M(101:900, 5), flipud(M(900:-1:101, 5)));
asserts.isequal(M(101:900, [7 1 3]), flipud(M(900:-1:101, [7 1 3])));
asserts.isequal(M(1:1000, :), M);
%=============================================================================
% empty ranges
E = A(3:2, :);
asserts.isequal(size(E), [0 4]);
asserts.isequal(class(E), 'double');
E = A(3:2, 2);
asserts.isequal(size(E), [0 1]);
E = A(2:4, []);
asserts.isequal(size(E), [3 0]);
E = int16(A);
E = E(3:2, :);
asserts.isequal(size(E), [0 4]);
asserts.isequal(class(E), 'int16');
%=============================================================================
% types
types = {'single', 'int8', 'uint8', 'int16', 'uint16', 'int32', 'uint32', 'int64', 'uint64'};
for t = 1:numel(types)
  B = cast(A, types{t});
  R = B(2:4, [1 3]);
  asserts.isequal(class(R), types{t});
  asserts.isequal(R, cast([2 12; 3 13; 4 14], types{t}));
end
L = logical(mod(A, 3) == 0);
R = L(2:4, :);
asserts.istrue(islogical(R));
asserts.isequal(R, logical(mod(A(2:4, :), 3) == 0));
CH = ['abcd'; 'efgh'; 'ijkl'];
asserts.isequal(CH(2:3, 2:4), ['fgh'; 'jkl']);
asserts.isequal(class(CH(2:3, :)), 'char');
%=============================================================================
% complex
Z = A + 1i * A;
R = Z(2:4, 2);
asserts.isequal(R, [7 + 7i; 8 + 8i; 9 + 9i]);
Z2 = complex(A, zeros(5, 4));
Z2(4, 1) = 4 + 2i;
R = Z2(1:3, :);
asserts.istrue(isreal(R));
asserts.isequal(R, A(1:3, :));
R = Z2(3:5, 1);
asserts.isequal(R, [3; 4 + 2i; 5]);
ZS = single(Z);
asserts.isequal(ZS(2:4, :), single(Z(2:4, :)));
asserts.isequal(class(ZS(2:4, :)), 'single');
%=============================================================================
% cells, strings, structs
C = num2cell(A);
R = C(2:3, [2 4]);
asserts.isequal(R, {7, 17; 8, 18});
S = string(A);
asserts.isequal(S(4:5, 1), ["4"; "5"]);
ST = struct('v', C);
R = ST(2:3, 4);
asserts.isequal(size(R), [2 1]);
asserts.isequal([R.v], [17 18]);
%=============================================================================
% sparse is not affected
SP = sparse(A);
R = SP(2:4, [1 3]);
asserts.istrue(issparse(R));
asserts.isequal(full(R), [2 12; 3 13; 4 14]);
%=============================================================================
% N-D arrays, fewer and more subscripts than dimensions
N = reshape(1:60, [5 4 3]);
R = N(2:4, :, 3);
asserts.isequal(R, [42 47 52 57; 43 48 53 58; 44 49 54 59]);
R = N(:, 2:3, 2);
asserts.isequal(R, reshape(26:35, [5 2]));
R = N(2:4, [4 1], [3 1]);
asserts.isequal(size(R), [3 2 2]);
asserts.isequal(R(:, :, 1), [57 42; 58 43; 59 44]);
asserts.isequal(R(:, :, 2), [17 2; 18 3; 19 4]);
R = N(2:3, 5:6);
asserts.isequal(R, [22 27; 23 28]);
R = N(2:3, :);
asserts.isequal(size(R), [2 12]);
asserts.isequal(R, [2:5:57; 3:5:58]);
R = A(2:3, 2, 1);
asserts.isequal(R, [7; 8]);
NB = rand(40, 30, 6);
asserts.isequal(NB(5:35, :, 2:5), flip(NB(35:-1:5, :, 2:5), 1));
asserts.isequal(NB(:, 3:28, 2:5), flip(NB(:, 28:-1:3, 2:5), 2));
%=============================================================================
% out of bounds
asserts.checkerror('A(3:6, :)', _('Index exceeds array bounds.'), 'Nelson:badsubscript');
asserts.checkerror('A(2:3, 5)', _('Index exceeds array bounds.'), 'Nelson:badsubscript');
asserts.checkerror('A(2:3, [1 5])', _('Index exceeds array bounds.'), 'Nelson:badsubscript');
asserts.checkerror('N(2:3, :, 4)', _('Index exceeds array bounds.'), 'Nelson:badsubscript');
asserts.checkerror('N(2:3, 13)', _('Index exceeds array bounds.'), 'Nelson:badsubscript');
%=============================================================================
