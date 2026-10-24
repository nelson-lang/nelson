%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
A = reshape(1:12, 3, 4);
B = (10:10:30)';
REF = A + repmat(B, 1, 4);
asserts.isequal(A + B, REF);
asserts.isequal(A - B, A - repmat(B, 1, 4));
%=============================================================================
A = 1:3;
B = (10:10:20)';
asserts.isequal(A + B, [11 12 13; 21 22 23]);
asserts.isequal(A .* B, [10 20 30; 20 40 60]);
asserts.isequal(B ./ A, [10 5 10 / 3; 20 10 20 / 3]);
asserts.isequal(A .\ B, [10 5 10 / 3; 20 10 20 / 3]);
asserts.isequal(A .^ B, [1 1024 59049; 1 1048576 3486784401]);
%=============================================================================
A = reshape(1:12, 4, 3);
B = reshape(101:109, 1, 3, 3);
R = A + B;
REF = zeros(4, 3, 3);
for k = 1:3
  REF(:, :, k) = A + repmat(B(:, :, k), 4, 1);
end
asserts.isequal(size(R), [4 3 3]);
asserts.isequal(R, REF);
%=============================================================================
R = ones(1, 0) + ones(3, 1);
asserts.isequal(size(R), [3 0]);
R = ones(1, 0) .* ones(3, 1);
asserts.isequal(size(R), [3 0]);
asserts.checkerror('ones(2, 0) + ones(3, 1);', [_('Size mismatch on arguments to arithmetic operator'), ' ', '+']);
asserts.checkerror('ones(2, 3) + ones(3, 1);', [_('Size mismatch on arguments to arithmetic operator'), ' ', '+']);
%=============================================================================
A = reshape(1:12, 4, 3);
B = reshape(1:9, 1, 3, 3);
R = A > B;
REF = false(4, 3, 3);
REQ = false(4, 3, 3);
for k = 1:3
  REF(:, :, k) = A > repmat(B(:, :, k), 4, 1);
  REQ(:, :, k) = A == repmat(B(:, :, k), 4, 1);
end
asserts.isequal(R, REF);
asserts.isequal(A == B, REQ);
%=============================================================================
A = logical([true false true]);
B = logical([true; false]);
asserts.isequal(A & B, logical([true false true; false false false]));
asserts.isequal(A | B, logical([true true true; true false true]));
%=============================================================================
A = uint8([1 2 3]);
B = uint8([1; 3]);
asserts.isequal(bitand(A, B), uint8([1 0 1; 1 2 3]));
asserts.isequal(bitor(A, B), uint8([1 3 3; 3 3 3]));
asserts.isequal(bitxor(A, B), uint8([0 3 2; 2 1 0]));
%=============================================================================
A = ["a", "b", "c"];
B = ["x"; "y"];
asserts.isequal(A + B, ["ax", "bx", "cx"; "ay", "by", "cy"]);
%=============================================================================
