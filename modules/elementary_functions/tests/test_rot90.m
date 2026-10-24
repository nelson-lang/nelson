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
asserts.isequal(nargin('rot90'), -1);
asserts.isequal(nargout('rot90'), -1);
asserts.istrue(isbuiltin('rot90'));
%=============================================================================
A = (1:5)';
%=============================================================================
R = rot90(A);
REF = 1:5;
asserts.isequal(R, REF);
%=============================================================================
R = rot90(A, 0);
REF = (1:5)';
asserts.isequal(R, REF);
%=============================================================================
R = rot90(A, 1);
REF = 1:5;
asserts.isequal(R, REF);
%=============================================================================
R = rot90(A, 2);
REF = [5; 4; 3; 2; 1];
asserts.isequal(R, REF);
%=============================================================================
R = rot90(A, 3);
REF = [5 4 3 2 1];
asserts.isequal(R, REF);
%=============================================================================
R = rot90(A, 4);
REF = (1:5)';
asserts.isequal(R, REF);
%=============================================================================
R = rot90(A, 40);
REF = (1:5)';
asserts.isequal(R, REF);
%=============================================================================
A = [1 2 3; 4 5 6];
%=============================================================================
R = rot90(A);
REF = [3 6;
  2 5;
  1 4];
asserts.isequal(R, REF);
%=============================================================================
R = rot90(A, 0);
REF = [1 2 3; 4 5 6];
asserts.isequal(R, REF);
%=============================================================================
R = rot90(A, 1);
REF = [3 6;
  2 5;
  1 4];
asserts.isequal(R, REF)
%=============================================================================
R = rot90(A, 2);
REF = [6 5 4
  3 2 1];
asserts.isequal(R, REF);
%=============================================================================
R = rot90(A, 3);
REF = [4 1;
  5 2;
  6 3];
asserts.isequal(R, REF);
%=============================================================================
R = rot90(A, 4);
REF = [1 2 3; 4 5 6];
asserts.isequal(R, REF)
%=============================================================================
R = rot90(A, 40);
REF = [1 2 3; 4 5 6];
asserts.isequal(R, REF)
%=============================================================================
R = rot90(ones(3, 0), 0);
REF = ones(3, 0);
asserts.isequal(R, REF);
%=============================================================================
R = rot90(ones(3, 0), 1);
REF = ones(0, 3);
asserts.isequal(R, REF);
%=============================================================================
R = rot90(ones(3, 0), 2);
REF = ones(3, 0);
asserts.isequal(R, REF);
%=============================================================================
R = rot90(ones(3, 0), 3);
REF = ones(0, 3);
asserts.isequal(R, REF);
%=============================================================================
A = [5; 4; 3; 2; 1] + i;
R = rot90(A, 2);
REF = [1; 2; 3; 4; 5] + i;
asserts.isequal(R, REF);
%=============================================================================
asserts.isequal(rot90([1 2; 3 4]), [2 4; 1 3]);
%=============================================================================
% fused rotate: every k matches the flip+permute definition, on a non-square
% matrix, across several types
A = reshape(1:12, 3, 4);
for kk = 0:4
  REF = A;
  for t = 1:mod(kk, 4)
    REF = permute(flip(REF, 2), [2 1]);
  end
  asserts.isequal(rot90(A, kk), REF);
end
asserts.isequal(class(rot90(single(A))), 'single');
asserts.isequal(class(rot90(int16(A), 3)), 'int16');
asserts.isequal(rot90(int8([1 2 3; 4 5 6])), int8([3 6; 2 5; 1 4]));
asserts.isequal(rot90(uint64([1 2 3; 4 5 6])), uint64([3 6; 2 5; 1 4]));
asserts.isequal(rot90(logical([1 0 1; 0 1 0])), logical([1 0; 0 1; 1 0]));
asserts.isequal(rot90('abc'), ['c'; 'b'; 'a']);
asserts.isequal(rot90(["ab" "cd"; "ef" "gh"]), ["cd" "gh"; "ab" "ef"]);
asserts.isequal(rot90({1, 'a'; true, 4}), {'a', 4; 1, true});
S = struct('a', num2cell([1 3; 2 4]));
R = rot90(S);
asserts.isequal(R(1, 1).a, 3);
asserts.isequal(R(1, 2).a, 4);
asserts.isequal(R(2, 1).a, 1);
asserts.isequal(R(2, 2).a, 2);
SP = sparse([1 2; 3 4]);
R = rot90(SP);
asserts.istrue(issparse(R));
asserts.isequal(full(R), [2 4; 1 3]);
%=============================================================================
% N-D input rotates each page in the first-two-dims plane
A3 = reshape(1:24, 2, 3, 4);
R3 = rot90(A3);
asserts.isequal(size(R3), [3 2 4]);
for p = 1:4
  asserts.isequal(R3(:, :, p), rot90(A3(:, :, p)));
end
% complex non-square, k = 1 and k = 3
Z = reshape((1:6) + 1i * (6:-1:1), 2, 3);
asserts.isequal(rot90(Z, 1), permute(flip(Z, 2), [2 1]));
REF3 = Z;
for t = 1:3
  REF3 = permute(flip(REF3, 2), [2 1]);
end
asserts.isequal(rot90(Z, 3), REF3);
%=============================================================================
asserts.checkerror('rot90([1 2; 3 4], [])', _('k must be a scalar.'));
asserts.checkerror('rot90([1 2; 3 4], [1 2])', _('k must be a scalar.'));
asserts.checkerror('rot90([1 2; 3 4], 1.5)', _('k must be an integer.'));
asserts.checkerror('rot90([1 2; 3 4], NaN)', _('k must be an integer.'));
asserts.checkerror('rot90([1 2; 3 4], Inf)', _('k must be an integer.'));
asserts.isequal(rot90([1 2; 3 4], true), [2 4; 1 3]);
asserts.isequal(rot90([1 2; 3 4], -1), [3 1; 4 2]);
asserts.isequal(rot90([1 2; 3 4], 1000001), [2 4; 1 3]);
asserts.isequal(rot90([1 2; 3 4], 'a'), [2 4; 1 3]);
asserts.isequal(rot90([1 2; 3 4], sparse(1)), [2 4; 1 3]);
asserts.isequal(rot90([1 2; 3 4], 1 + 0i), [2 4; 1 3]);
asserts.checkerror('rot90([1 2; 3 4], 1 + 2i)', sprintf(_('%s must be real.'), 'Argument'));
asserts.checkerror('rot90([1 2; 3 4], "1")', _('Invalid data type. Argument must be numeric, char, or logical.'));
didError = false;
try
  rot90(table([1; 2], [3; 4]));
catch
  didError = true;
end
asserts.istrue(didError);
%=============================================================================
