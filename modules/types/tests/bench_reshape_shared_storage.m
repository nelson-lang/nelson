%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--RELEASE ONLY-->
%=============================================================================
% Bench: reshape of a shared array reuses its data instead of copying it.
A = ones(4e7, 1, 'int64');
R = 20;
tic();
for r = 1:R
  B = reshape(A, 4e3, []);
end
t = toc();
asserts.isequal(size(B), [4e3, 1e4]);
fprintf('  reshape shared int64 320 MB:  %0.3f ms/call\n', t / R * 1e3);
asserts.istrue(t / R < 5e-3);
%=============================================================================
% the first write to the reshaped value pays the copy once, not the reshape
tic();
B(1) = 2;
t = toc();
fprintf('  first write after reshape:     %0.1f ms\n', t * 1e3);
asserts.isequal(A(1), int64(1));
clear B
%=============================================================================
% the original value released: no copy at all on the first write
tic();
A = reshape(A, 1, []);
A(1) = 3;
t = toc();
fprintf('  self reshape + write:          %0.3f ms\n', t * 1e3);
asserts.istrue(t < 5e-3);
clear A
%=============================================================================
% vector transpose and singleton permute share the data too
v = rand(1, 1e7);
tic();
for r = 1:R
  w = v.';
end
t = toc();
fprintf('  transpose vector 1e7:          %0.3f ms/call\n', t / R * 1e3);
asserts.istrue(t / R < 5e-3);
P = rand(1, 2000, 3000);
tic();
for r = 1:R
  Q = permute(P, [2 3 1]);
end
t = toc();
fprintf('  permute singleton 6e6:         %0.3f ms/call\n', t / R * 1e3);
asserts.istrue(t / R < 5e-3);
asserts.isequal(size(Q), [2000 3000]);
clear v w P Q
%=============================================================================
S = sprand(4000, 4000, 0.01);
tic();
for r = 1:R
  T = reshape(S, 2000, 8000);
end
t = toc();
fprintf('  sparse reshape nnz %d:     %0.3f ms/call\n', nnz(S), t / R * 1e3);
asserts.isequal(nnz(T), nnz(S));
clear S T
%=============================================================================
% small arrays: reshape stays cheap
x = rand(3, 4);
N = 1e5;
tic();
for k = 1:N
  y = reshape(x, 4, 3);
end
t = toc();
fprintf('  reshape 3x4:                   %0.3f us/call\n', t / N * 1e6);
%=============================================================================
tic();
for k = 1:N
  y = reshape(x, 4, 3);
  y(1) = k;
end
t = toc();
fprintf('  reshape 3x4 + write:           %0.3f us/call\n', t / N * 1e6);
asserts.isequal(y(1), N);
%=============================================================================
