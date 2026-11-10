%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% reshape of a shared array reuses its data: a write to one value never shows in the other
%=============================================================================
A = 1:12;
B = reshape(A, 3, 4);
A(1) = 100;
assert_isequal(B, reshape(1:12, 3, 4));
assert_isequal(A(1), 100);
B(2, 2) = -1;
assert_isequal(A, [100, 2:12]);
assert_isequal(B(5), -1);
%=============================================================================
% the reshaped value is written first
A = int64(1:12);
B = reshape(A, 2, 6);
B(:) = 0;
assert_isequal(A, int64(1:12));
assert_isequal(B, zeros(2, 6, 'int64'));
%=============================================================================
% reshape of a reshaped value
A = 1:24;
B = reshape(A, 4, 6);
C = reshape(B, 2, 3, 4);
D = C;
C(1) = 0;
B(end) = 0;
assert_isequal(A, 1:24);
assert_isequal(D, reshape(1:24, 2, 3, 4));
assert_isequal(C(1), 0);
assert_isequal(B(24), 0);
assert_isequal(C(24), 24);
%=============================================================================
% the original value released: the reshaped one is written without a copy
A = rand(4, 5);
ref = A;
A = reshape(A, 5, 4);
A(3) = 42;
assert_isequal(A(3), 42);
assert_isequal(ref(3) == 42, false);
assert_isequal(size(A), [5 4]);
%=============================================================================
% growth after reshape
A = 1:6;
B = reshape(A, 6, 1);
B(end + 1) = 7;
assert_isequal(A, 1:6);
assert_isequal(B, (1:7)');
A(end + 1) = 8;
assert_isequal(A, [1:6, 8]);
assert_isequal(B, (1:7)');
%=============================================================================
% a grown vector keeps spare room: reshape then grow both values
v = [];
for k = 1:20
  v(k) = k; %#ok<NLS0039>
end
w = reshape(v, 20, 1);
v(21) = 21;
w(21) = -21;
assert_isequal(v, 1:21);
assert_isequal(w, [(1:20)'; -21]);
%=============================================================================
% row append fast path on a reshaped value
x = reshape([1 2; 3 4], 1, 4);
y = x;
for k = 1:3
  x = [x k]; %#ok<NLS0039>
end
assert_isequal(x, [1 3 2 4 1 2 3]);
assert_isequal(y, [1 3 2 4]);
%=============================================================================
% cell arrays
c = {1, 'a', [1 2 3], {4}, 5, 6};
d = reshape(c, 2, 3);
c{3} = 'changed';
d{1, 1} = 'other';
assert_isequal(c, {1, 'a', 'changed', {4}, 5, 6});
assert_isequal(d, reshape({'other', 'a', [1 2 3], {4}, 5, 6}, 2, 3));
%=============================================================================
% struct arrays and new fields
s = struct('f', {1, 2, 3, 4});
t = reshape(s, 2, 2);
t(2, 2).f = 40;
t(1).g = 'new';
assert_isequal([s.f], [1 2 3 4]);
assert_isequal(isfield(s, 'g'), false);
assert_isequal(t(4).f, 40);
assert_isequal(t(1).g, 'new');
s(1).f = 10;
assert_isequal(t(1).f, 1);
%=============================================================================
% strings, characters, logical, complex
str = ["a", "b", "c", "d"];
str2 = reshape(str, 2, 2);
str2(1) = "z";
assert_isequal(str, ["a", "b", "c", "d"]);
ch = 'abcdef';
ch2 = reshape(ch, 2, 3);
ch(1) = 'Z';
assert_isequal(ch2, ['ace'; 'bdf']);
L = true(1, 4);
L2 = reshape(L, 2, 2);
L2(1) = false;
assert_isequal(L, true(1, 4));
z = complex(1:4, 4:-1:1);
z2 = reshape(z, 2, 2);
z2(4) = 0;
assert_isequal(z, complex(1:4, 4:-1:1));
assert_isequal(z2(:).', complex([1 2 3 0], [4 3 2 0]));
%=============================================================================
% sparse keeps its own reshape
S = sparse([1 0 2 0]);
S2 = reshape(S, 2, 2);
S2(1, 1) = 9;
assert_isequal(full(S), [1 0 2 0]);
assert_isequal(full(S2), [9 2; 0 0]);
%=============================================================================
% colon reshape and in-place operations inside functions
A = magic(4);
B = A(:);
B(1) = 0;
assert_isequal(A, magic(4));
A = magic(4);
r = reshape_shared_helper_modify(A);
assert_isequal(A, magic(4));
assert_isequal(r, reshape(magic(4) * 2 + 1, 2, 8));
%=============================================================================
x = 1:6;
[y, z] = reshape_shared_helper_operator(x);
assert_isequal(x, 1:6);
assert_isequal(y, [5 9; 7 11; 9 13]);
assert_isequal(z, reshape(1:6, 3, 2));
%=============================================================================
% reshape back to the original shape
A = 1:8;
B = reshape(reshape(A, 2, 4), 1, 8);
B(8) = 0;
assert_isequal(A, 1:8);
assert_isequal(B, [1:7, 0]);
% vector transposes and singleton permutes share data but stay separate values
v = 1:5;
w = v.';
x = v';
w(1) = 10;
x(2) = 20;
assert_isequal(v, 1:5);
assert_isequal(w, [10; 2; 3; 4; 5]);
assert_isequal(x, [1; 20; 3; 4; 5]);
z = complex(1:3, 1:3);
assert_isequal(z', complex([1; 2; 3], -[1; 2; 3]));
assert_isequal(z.', complex([1; 2; 3], [1; 2; 3]));
c = {1, 'a', 3};
c2 = c';
c2{1} = 0;
assert_isequal(c, {1, 'a', 3});
assert_isequal(size(c2), [3 1]);
s = string({'a', 'b'})';
assert_isequal(s, ["a"; "b"]);
A = reshape(1:6, 1, 2, 3);
P = permute(A, [2 3 1]);
P(1) = 0;
assert_isequal(A, reshape(1:6, 1, 2, 3));
assert_isequal(P, [0 3 5; 2 4 6]);
assert_isequal(permute(reshape(1:6, 2, 1, 3), [1 3 2]), [1 3 5; 2 4 6]);
assert_isequal(permute(reshape(1:6, 2, 3), [2 1]), [1 2; 3 4; 5 6]);
X = reshape(1:24, 2, 3, 4);
Y = permute(X, [3 1 2]);
assert_isequal(size(Y), [4 2 3]);
assert_isequal(Y(3, 2, 1), X(2, 1, 3));
assert_isequal(Y(4, 1, 3), X(1, 3, 4));
B = shiftdim(reshape(1:6, 1, 1, 6));
B(end) = 0;
assert_isequal(B, [1; 2; 3; 4; 5; 0]);
%=============================================================================
% sparse reshape keeps every value at its linear position
S = sparse([1 0 0 4; 0 2 0 0; 3 0 0 5]);
for shape = {[2 6], [6 2], [1 12], [12 1], [4 3]}
  dims = shape{1};
  R = reshape(S, dims(1), dims(2));
  assert_istrue(issparse(R));
  assert_isequal(full(R), reshape(full(S), dims(1), dims(2)));
end
assert_isequal(nnz(reshape(sparse(3, 4), 6, 2)), 0);
L = reshape(sparse(logical([1 0; 0 1; 1 1])), 2, 3);
assert_isequal(full(L), logical([1 1 1; 0 0 1]));
C = reshape(sparse([1i 0; 0 2]), 1, 4);
assert_isequal(full(C), [1i 0 0 2]);
%=============================================================================
% values reshaped on background workers
A = reshape(1:400, 20, 20);
pool = backgroundPool();
f = parfeval(pool, @(m) reshape(m, 1, []), 1, A);
g = parfeval(pool, @(m) reshape(m, 40, 10) * 2, 1, A);
R1 = fetchOutputs(f);
R2 = fetchOutputs(g);
R1(1) = -1;
A(2) = -2;
assert_isequal(R1, [-1, 2:400]);
assert_isequal(R2, reshape(2:2:800, 40, 10));
assert_isequal(A(1:3), [1 -2 3]);
% shared data with another shape is not the same value when appending
assert_isequal(reshape_shared_helper_append(), [true, true]);
%=============================================================================
function r = reshape_shared_helper_append()
  r = [false, false];
  z = (1:4)';
  x = reshape(z, 1, 4);
  try
    x = [z 5];
  catch
    r(1) = true;
  end
  x = 1:4;
  h = reshape(x, 4, 1);
  try
    x = [h 5];
  catch
    r(2) = true;
  end
end
%=============================================================================
function r = reshape_shared_helper_modify(m)
  r = reshape(m, 2, 8);
  r = r * 2;
  r(:) = r(:) + 1;
  m(1) = -1; %#ok<NLS0036>
end
%=============================================================================
function [y, z] = reshape_shared_helper_operator(x)
  y = reshape(x, 3, 2);
  z = y;
  a = [1 4; 2 5; 3 6];
  b = [4 5; 5 6; 6 7];
  y = a + b;
end
%=============================================================================
