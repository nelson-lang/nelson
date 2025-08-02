%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
test_h5save_file = [tempdir(), 'test_h5save_double.nh5'];
if isfile(test_h5save_file)
  rmfile(test_h5save_file);
end
%=============================================================================
% double
%=============================================================================
A = eye(3, 4);
savenh5(test_h5save_file, 'A');
assert_istrue(isfile(test_h5save_file));
schema_version = h5readatt(test_h5save_file, '/', 'NELSON_schema');
assert_isequal(schema_version, int32(1));
R = h5read(test_h5save_file, '/A');
assert_isequal(R, A);
R = h5readatt(test_h5save_file, '/A', 'NELSON_class');
assert_isequal(R, 'double');
R = h5readatt(test_h5save_file, '/A', 'NELSON_dimensions');
assert_isequal(R, uint64(size(A)'));
%=============================================================================
B = 1;
savenh5(test_h5save_file, 'B');
assert_istrue(isfile(test_h5save_file));
schema_version = h5readatt(test_h5save_file, '/', 'NELSON_schema');
assert_isequal(schema_version, int32(1));
R = h5read(test_h5save_file, '/B');
assert_isequal(R, B);
R = h5readatt(test_h5save_file, '/B', 'NELSON_class');
assert_isequal(R, 'double');
R = h5readatt(test_h5save_file, '/B', 'NELSON_dimensions');
assert_isequal(R, uint64(size(B)'));
%=============================================================================
C = eye(3, 4) + i;
savenh5(test_h5save_file, 'C');
R = h5readatt(test_h5save_file, '/C', 'NELSON_class');
assert_isequal(R, 'double');
R = h5readatt(test_h5save_file, '/C', 'NELSON_complex');
assert_isequal(R, uint8(1));
R = h5readatt(test_h5save_file, '/C', 'NELSON_dimensions');
assert_isequal(R, uint64(size(C)'));
R = h5read(test_h5save_file, '/C');
REF = struct();
REF.real = eye(3, 4);
REF.imag = ones(3, 4);
assert_isequal(R, REF);
%=============================================================================
D = 1 + 2i;
savenh5(test_h5save_file, 'D');
R = h5readatt(test_h5save_file, '/D', 'NELSON_class');
assert_isequal(R, 'double');
R = h5readatt(test_h5save_file, '/D', 'NELSON_complex');
assert_isequal(R, uint8(1));
R = h5readatt(test_h5save_file, '/D', 'NELSON_dimensions');
assert_isequal(R, uint64(size(D)'));
R = h5read(test_h5save_file, '/D');
REF = struct();
REF.real = 1;
REF.imag = 2;
assert_isequal(R, REF);
%=============================================================================
E = ones(3, 0);
savenh5(test_h5save_file, 'E');
R = h5readatt(test_h5save_file, '/E', 'NELSON_class');
assert_isequal(R, 'double');
R = h5readatt(test_h5save_file, '/E', 'NELSON_dimensions');
assert_isequal(R, uint64(size(E)'));
R = h5readatt(test_h5save_file, '/E', 'NELSON_empty');
assert_isequal(R, uint8(1));
R = h5read(test_h5save_file, '/E');
assert_isequal(R, 0);
%=============================================================================
F = ones(0, 4);
savenh5(test_h5save_file, 'F');
R = h5readatt(test_h5save_file, '/F', 'NELSON_class');
assert_isequal(R, 'double');
R = h5readatt(test_h5save_file, '/F', 'NELSON_dimensions');
assert_isequal(R, uint64(size(F)'));
R = h5readatt(test_h5save_file, '/F', 'NELSON_empty');
assert_isequal(R, uint8(1));
R = h5read(test_h5save_file, '/F');
assert_isequal(R, 0);
%=============================================================================
G = ones(3, 0, 2, 4);
savenh5(test_h5save_file, 'G');
R = h5readatt(test_h5save_file, '/G', 'NELSON_class');
assert_isequal(R, 'double');
R = h5readatt(test_h5save_file, '/G', 'NELSON_dimensions');
assert_isequal(R, uint64(size(G)'));
R = h5readatt(test_h5save_file, '/G', 'NELSON_empty');
assert_isequal(R, uint8(1));
R = h5read(test_h5save_file, '/G');
assert_isequal(R, 0);
%=============================================================================
H = ones(3, 0, 0, 0);
savenh5(test_h5save_file, 'H');
R = h5readatt(test_h5save_file, '/H', 'NELSON_class');
assert_isequal(R, 'double');
R = h5readatt(test_h5save_file, '/H', 'NELSON_dimensions');
assert_isequal(R, uint64(size(H)'));
R = h5readatt(test_h5save_file, '/H', 'NELSON_empty');
assert_isequal(R, uint8(1));
R = h5read(test_h5save_file, '/H');
assert_isequal(R, 0);
%=============================================================================
K = ones(3, 0, 2, 4) + 4i;
savenh5(test_h5save_file, 'K');
R = h5readatt(test_h5save_file, '/K', 'NELSON_class');
assert_isequal(R, 'double');
R = h5readatt(test_h5save_file, '/K', 'NELSON_dimensions');
assert_isequal(R, uint64(size(K)'));
R = h5readatt(test_h5save_file, '/K', 'NELSON_empty');
assert_isequal(R, uint8(1));
R = h5read(test_h5save_file, '/K');
assert_isequal(R, 0);
%=============================================================================
L = reshape(1:18,3,2,3);
savenh5(test_h5save_file, 'L');
R = h5readatt(test_h5save_file, '/L', 'NELSON_class');
assert_isequal(R, 'double');
R = h5readatt(test_h5save_file, '/L', 'NELSON_dimensions');
assert_isequal(R, uint64(size(L)'));
R = h5read(test_h5save_file, '/L');
assert_isequal(R, L);
%=============================================================================
M = reshape(1:18, 3, 2, 3) + 2i;
savenh5(test_h5save_file, 'M');
R = h5readatt(test_h5save_file, '/M', 'NELSON_class');
assert_isequal(R, 'double');
R = h5readatt(test_h5save_file, '/M', 'NELSON_dimensions');
assert_isequal(R, uint64(size(M)'));
R = h5readatt(test_h5save_file, '/M', 'NELSON_complex');
assert_isequal(R, uint8(1));
R = h5read(test_h5save_file, '/M');
REF = struct();
REF.real = reshape(1:18, 3, 2, 3);
REF.imag = ones(3, 2, 3) .* 2;
assert_isequal(R, REF);
%=============================================================================
N = sparse(4, 5, 9, 6, 7, 19);
savenh5(test_h5save_file, 'N');
R = h5readatt(test_h5save_file, '/N', 'NELSON_class');
assert_isequal(R, 'double');
R = h5readatt(test_h5save_file, '/N', 'NELSON_dimensions');
assert_isequal(R, uint64(size(N)'));
R = h5readatt(test_h5save_file, '/N', 'NELSON_sparse');
assert_isequal(R, uint8(1));
R = h5readatt(test_h5save_file, '/N', 'NELSON_nzmax');
assert_isequal(R, uint64(19));
R = h5read(test_h5save_file, '/N/data');
assert_isequal(R, 9);
R = h5read(test_h5save_file, '/N/ir');
assert_isequal(R, uint64(4));
R = h5read(test_h5save_file, '/N/jc');
assert_isequal(R, uint64(5));
%=============================================================================
O = sparse(eye(0, 4));
savenh5(test_h5save_file, 'O');
R = h5readatt(test_h5save_file, '/O', 'NELSON_nzmax');
assert_isequal(R, uint64(1));
R = h5readatt(test_h5save_file, '/O', 'NELSON_dimensions');
assert_isequal(R, uint64(size(O)'));
R = h5readatt(test_h5save_file, '/O', 'NELSON_sparse');
assert_isequal(R, uint8(1));
R = h5readatt(test_h5save_file, '/O', 'NELSON_class');
assert_isequal(R, 'double');
%=============================================================================
P = sparse(eye(5, 0));
savenh5(test_h5save_file, 'P');
R = h5readatt(test_h5save_file, '/P', 'NELSON_nzmax');
assert_isequal(R, uint64(1));
R = h5readatt(test_h5save_file, '/P', 'NELSON_dimensions');
assert_isequal(R, uint64(size(P)'));
R = h5readatt(test_h5save_file, '/P', 'NELSON_sparse');
assert_isequal(R, uint8(1));
R = h5readatt(test_h5save_file, '/P', 'NELSON_class');
assert_isequal(R, 'double');
%=============================================================================
Q = sparse(eye(5, 4));
savenh5(test_h5save_file, 'Q');
R = h5readatt(test_h5save_file, '/Q', 'NELSON_nzmax');
assert_isequal(R, uint64(4));
R = h5readatt(test_h5save_file, '/Q', 'NELSON_dimensions');
assert_isequal(R, uint64(size(Q)'));
R = h5readatt(test_h5save_file, '/Q', 'NELSON_sparse');
assert_isequal(R, uint8(1));
R = h5readatt(test_h5save_file, '/Q', 'NELSON_class');
assert_isequal(R, 'double');
R = h5read(test_h5save_file, '/Q/data');
assert_isequal(R, [1 1 1 1]);
R = h5read(test_h5save_file, '/Q/ir');
assert_isequal(R, uint64([1 2 3 4]));
R = h5read(test_h5save_file, '/Q/jc');
assert_isequal(R, uint64([1 2 3 4]));
%=============================================================================
S = sparse(eye(5, 4) + 2i);
[I, J, V] = IJV(S);
savenh5(test_h5save_file, 'S');
R = h5readatt(test_h5save_file, '/S', 'NELSON_nzmax');
assert_isequal(R, uint64(20));
R = h5readatt(test_h5save_file, '/S', 'NELSON_dimensions');
assert_isequal(R, uint64(size(S)'));
R = h5readatt(test_h5save_file, '/S', 'NELSON_sparse');
assert_isequal(R, uint8(1));
R = h5readatt(test_h5save_file, '/S', 'NELSON_class');
assert_isequal(R, 'double');
R = h5readatt(test_h5save_file, '/S', 'NELSON_complex');
assert_isequal(R, uint8(1));
R = h5read(test_h5save_file, '/S/data');
REF = struct();
REF.real = [1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0];
REF.imag = ones(1, 20) * 2;
R = h5read(test_h5save_file, '/S/ir');
assert_isequal(R, uint64(I'));
R = h5read(test_h5save_file, '/S/jc');
assert_isequal(R, uint64(J'));
%=============================================================================
