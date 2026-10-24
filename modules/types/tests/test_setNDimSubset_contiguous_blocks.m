%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
% A(a:b, J, ...) = B writes one contiguous block per column; the reference is
% built with scalar assignments, which run the element-wise path.
%=============================================================================
A = reshape(1:48, 6, 8);
B = -reshape(1:12, 3, 4);
R = A; R(2:4, 3:6) = B;
asserts.isequal(R, scalar_assign(A, 2:4, 3:6, B));
R = A; R(2:4, [8 1 5 2]) = B;
asserts.isequal(R, scalar_assign(A, 2:4, [8 1 5 2], B));
R = A; R(4:6, [2 2 7 3]) = B;
asserts.isequal(R, scalar_assign(A, 4:6, [2 2 7 3], B));
R = A; R(1:6, 2:3) = -reshape(1:12, 6, 2);
asserts.isequal(R, scalar_assign(A, 1:6, 2:3, -reshape(1:12, 6, 2)));
R = A; R(:, [5 1]) = -reshape(1:12, 6, 2);
asserts.isequal(R, scalar_assign(A, 1:6, [5 1], -reshape(1:12, 6, 2)));
R = A; R(3:4, :) = -reshape(1:16, 2, 8);
asserts.isequal(R, scalar_assign(A, 3:4, 1:8, -reshape(1:16, 2, 8)));
R = A; R(5, 2:5) = [-1 -2 -3 -4];
asserts.isequal(R, scalar_assign(A, 5, 2:5, [-1 -2 -3 -4]));
R = A; R(end - 2:end, end) = [-1; -2; -3];
asserts.isequal(R, scalar_assign(A, 4:6, 8, [-1; -2; -3]));
%=============================================================================
% first subscript not an ascending run: element-wise path, same result
R = A; R([4 3 2], 3:6) = B;
asserts.isequal(R, scalar_assign(A, [4 3 2], 3:6, B));
R = A; R([2 4 6], 3:6) = B;
asserts.isequal(R, scalar_assign(A, [2 4 6], 3:6, B));
% scalar right-hand side
R = A; R(2:4, 3:6) = 0;
asserts.isequal(R, scalar_assign(A, 2:4, 3:6, zeros(3, 4)));
%=============================================================================
% Jacobi-style update on a padded array (shifted slices)
q = zeros(10, 12);
rhs = reshape(1:80, 8, 10);
I = 2:9; J = 2:11;
for it = 1:3
  q(I, J) = (q(I - 1, J) + q(I + 1, J) + q(I, J - 1) + q(I, J + 1)) / 4 + rhs;
end
ref = zeros(10, 12);
for it = 1:3
  prev = ref;
  for jj = J
    for ii = I
      ref(ii, jj) = (prev(ii - 1, jj) + prev(ii + 1, jj) + prev(ii, jj - 1) + prev(ii, jj + 1)) / 4 + rhs(ii - 1, jj - 1);
    end
  end
end
asserts.isequal(q, ref);
%=============================================================================
% N-D: run on the first dimension, ':' on a later one
C = reshape(1:60, 3, 4, 5);
R = C; R(2:3, :, 4) = -reshape(1:8, 2, 4);
E = C;
v = -reshape(1:8, 2, 4);
for jj = 1:4
  for ii = 1:2
    E(ii + 1, jj, 4) = v(ii, jj);
  end
end
asserts.isequal(R, E);
R = C; R(1:2, 3, [5 1]) = -[1 3; 2 4];
E = C; E(1, 3, 5) = -1; E(2, 3, 5) = -2; E(1, 3, 1) = -3; E(2, 3, 1) = -4;
asserts.isequal(R, E);
%=============================================================================
% growth: the destination is resized before the blocks are written
R = A; R(5:7, 9:10) = -[1 4; 2 5; 3 6];
asserts.isequal(size(R), [7 10]);
asserts.isequal(R(5:7, 9:10), -[1 4; 2 5; 3 6]);
asserts.isequal(R(1:6, 1:8), A);
asserts.isequal(R(7, 1:8), zeros(1, 8));
asserts.isequal(R(1:4, 9:10), zeros(4, 2));
%=============================================================================
% classes
Ai = int16(A);
R = Ai; R(2:4, 3:6) = int16(B);
asserts.isequal(class(R), 'int16');
asserts.isequal(R, int16(scalar_assign(A, 2:4, 3:6, B)));
Ac = A + 1i;
R = Ac; R(2:4, 3:6) = B * 1i;
asserts.isequal(R, scalar_assign(Ac, 2:4, 3:6, B * 1i));
R = A; R(2:4, 3:6) = B * 1i;
asserts.isequal(R, scalar_assign(complex(A), 2:4, 3:6, B * 1i));
As = single(A);
R = As; R(2:4, 3:6) = B;
asserts.isequal(class(R), 'single');
asserts.isequal(R, single(scalar_assign(A, 2:4, 3:6, B)));
L = false(4, 5);
R = L; R(2:3, 4:5) = true(2, 2);
asserts.isequal(R, logical([0 0 0 0 0; 0 0 0 1 1; 0 0 0 1 1; 0 0 0 0 0]));
S = repmat('abcd', 3, 1);
R = S; R(2:3, 2:3) = ['XY'; 'ZW'];
asserts.isequal(R, ['abcd'; 'aXYd'; 'aZWd']);
K = num2cell(reshape(1:12, 3, 4));
R = K; R(2:3, [4 1]) = {'a', 'b'; 'c', 'd'};
asserts.isequal(R, {1, 4, 7, 10; 'b', 5, 8, 'a'; 'd', 6, 9, 'c'});
%=============================================================================
% size mismatch keeps its error
assert_checkerror('R = A; R(2:4, 3:6) = ones(2, 3);', _('Size mismatch in assignment A(I1,I2,...,In) = B.'), 'Nelson:dimagree:sizeMismatchAssignmentAI1I2');
%=============================================================================
function R = scalar_assign(A, I, J, B)
  R = A;
  for jj = 1:numel(J)
    for ii = 1:numel(I)
      R(I(ii), J(jj)) = B(ii, jj);
    end
  end
end
%=============================================================================
