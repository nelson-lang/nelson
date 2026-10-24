%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
test_h5save_file = [tempname(), '.nh5'];
if isfile(test_h5save_file)
  rmfile(test_h5save_file);
end
%=============================================================================
% logical
%=============================================================================
A = logical(eye(3, 4));
savenh5(test_h5save_file, 'A');
schema_version = h5readatt(test_h5save_file, '/', 'NELSON_schema');
asserts.isequal(schema_version, int32(1));
R = h5read(test_h5save_file, '/A');
asserts.isequal(R, uint8(A));
R = h5readatt(test_h5save_file, '/A', 'NELSON_class');
asserts.isequal(R, class(A));
R = h5readatt(test_h5save_file, '/A', 'NELSON_dimensions');
asserts.isequal(R, uint64(size(A)'));
%=============================================================================
A = logical(1);
savenh5(test_h5save_file, 'A');
R = h5read(test_h5save_file, '/A');
asserts.isequal(R, uint8(A));
R = h5readatt(test_h5save_file, '/A', 'NELSON_class');
asserts.isequal(R, class(A));
R = h5readatt(test_h5save_file, '/A', 'NELSON_dimensions');
asserts.isequal(R, uint64(size(A)'));
%=============================================================================
A = logical(ones(3, 0));
savenh5(test_h5save_file, 'A');
R = h5readatt(test_h5save_file, '/A', 'NELSON_class');
asserts.isequal(R, 'logical');
R = h5readatt(test_h5save_file, '/A', 'NELSON_dimensions');
asserts.isequal(R, uint64(size(A)'));
R = h5readatt(test_h5save_file, '/A', 'NELSON_empty');
asserts.isequal(R, uint8(1));
R = h5read(test_h5save_file, '/A');
asserts.isequal(R, uint8(0));
%=============================================================================
A = logical(ones(0, 4));
savenh5(test_h5save_file, 'A');
R = h5readatt(test_h5save_file, '/A', 'NELSON_class');
asserts.isequal(R, 'logical');
R = h5readatt(test_h5save_file, '/A', 'NELSON_dimensions');
asserts.isequal(R, uint64(size(A)'));
R = h5readatt(test_h5save_file, '/A', 'NELSON_empty');
asserts.isequal(R, uint8(1));
R = h5read(test_h5save_file, '/A');
asserts.isequal(R, uint8(0));
%=============================================================================
A = logical(ones(3, 0, 2, 4));
savenh5(test_h5save_file, 'A');
R = h5readatt(test_h5save_file, '/A', 'NELSON_class');
asserts.isequal(R, 'logical');
R = h5readatt(test_h5save_file, '/A', 'NELSON_dimensions');
asserts.isequal(R, uint64(size(A)'));
R = h5readatt(test_h5save_file, '/A', 'NELSON_empty');
asserts.isequal(R, uint8(1));
R = h5read(test_h5save_file, '/A');
asserts.isequal(R, uint8(0));
%=============================================================================
A = logical(ones(3, 0, 0, 0));
savenh5(test_h5save_file, 'A');
R = h5readatt(test_h5save_file, '/A', 'NELSON_class');
asserts.isequal(R, 'logical');
R = h5readatt(test_h5save_file, '/A', 'NELSON_dimensions');
asserts.isequal(R, uint64(size(A)'));
R = h5readatt(test_h5save_file, '/A', 'NELSON_empty');
asserts.isequal(R, uint8(1));
R = h5read(test_h5save_file, '/A');
asserts.isequal(R, uint8(0));
%=============================================================================
A = logical(reshape(1:18, 3, 2, 3));
savenh5(test_h5save_file, 'A');
R = h5readatt(test_h5save_file, '/A', 'NELSON_class');
asserts.isequal(R, 'logical');
R = h5readatt(test_h5save_file, '/A', 'NELSON_dimensions');
asserts.isequal(R, uint64(size(A)'));
R = h5read(test_h5save_file, '/A');
asserts.isequal(R, uint8(A));
%=============================================================================
A = sparse(4, 5, true, 6, 7, 19);
savenh5(test_h5save_file, 'A');
R = h5readatt(test_h5save_file, '/A', 'NELSON_class');
asserts.isequal(R, 'logical');
R = h5readatt(test_h5save_file, '/A', 'NELSON_dimensions');
asserts.isequal(R, uint64(size(A)'));
R = h5readatt(test_h5save_file, '/A', 'NELSON_sparse');
asserts.isequal(R, uint8(1));
R = h5readatt(test_h5save_file, '/A', 'NELSON_nzmax');
asserts.isequal(R, uint64(19));
R = h5read(test_h5save_file, '/A/data');
asserts.isequal(R, uint8(1));
R = h5read(test_h5save_file, '/A/ir');
asserts.isequal(R, uint64(4));
R = h5read(test_h5save_file, '/A/jc');
asserts.isequal(R, uint64(5));
%=============================================================================
A = logical(sparse(eye(0, 4)));
savenh5(test_h5save_file, 'A');
R = h5readatt(test_h5save_file, '/A', 'NELSON_nzmax');
asserts.isequal(R, uint64(1));
R = h5readatt(test_h5save_file, '/A', 'NELSON_dimensions');
asserts.isequal(R, uint64(size(A)'));
R = h5readatt(test_h5save_file, '/A', 'NELSON_sparse');
asserts.isequal(R, uint8(1));
R = h5readatt(test_h5save_file, '/A', 'NELSON_class');
asserts.isequal(R, 'logical');
%=============================================================================
A = logical(sparse(eye(5, 0)));
savenh5(test_h5save_file, 'A');
R = h5readatt(test_h5save_file, '/A', 'NELSON_nzmax');
asserts.isequal(R, uint64(1));
R = h5readatt(test_h5save_file, '/A', 'NELSON_dimensions');
asserts.isequal(R, uint64(size(A)'));
R = h5readatt(test_h5save_file, '/A', 'NELSON_sparse');
asserts.isequal(R, uint8(1));
R = h5readatt(test_h5save_file, '/A', 'NELSON_class');
asserts.isequal(R, 'logical');
%=============================================================================
A = logical(sparse(eye(5, 4)));
savenh5(test_h5save_file, 'A');
R = h5readatt(test_h5save_file, '/A', 'NELSON_nzmax');
asserts.isequal(R, uint64(4));
R = h5readatt(test_h5save_file, '/A', 'NELSON_dimensions');
asserts.isequal(R, uint64(size(A)'));
R = h5readatt(test_h5save_file, '/A', 'NELSON_sparse');
asserts.isequal(R, uint8(1));
R = h5readatt(test_h5save_file, '/A', 'NELSON_class');
asserts.isequal(R, 'logical');
R = h5read(test_h5save_file, '/A/data');
asserts.isequal(R, uint8([1 1 1 1]));
R = h5read(test_h5save_file, '/A/ir');
asserts.isequal(R, uint64([1 2 3 4]));
R = h5read(test_h5save_file, '/A/jc');
asserts.isequal(R, uint64([1 2 3 4]));
%=============================================================================
rmfile(test_h5save_file);
%=============================================================================
