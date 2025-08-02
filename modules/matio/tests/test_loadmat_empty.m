%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
mat_dir = [fileparts(nfilename('fullpathext'),'path'), '/mat/'];
%=============================================================================
clear emptyNDarray
loadmat([mat_dir, 'test_emptyNDarray_v6.1_GLNX86.mat']);
assert_isequal(size(emptyNDarray),[0 0 0])
%=============================================================================
clear emptyNDarray
loadmat([mat_dir, 'test_emptyNDarray_v7.1_GLNX86.mat']);
assert_isequal(size(emptyNDarray),[0 0 0])
%=============================================================================
clear emptydoublematrix
loadmat([mat_dir, 'test_emptydoublematrix_v4.2c_GLNX86.mat']);
emptydoublematrix_ref = zeros(0, 0);
assert_isequal(emptydoublematrix, emptydoublematrix_ref);
%=============================================================================
clear emptydoublematrix
loadmat([mat_dir, 'test_emptydoublematrix_v6.1_GLNX86.mat']);
assert_isequal(emptydoublematrix, emptydoublematrix_ref);
%=============================================================================
clear emptydoublematrix
loadmat([mat_dir, 'test_emptydoublematrix_v7.1_GLNX86.mat']);
assert_isequal(emptydoublematrix, emptydoublematrix_ref);
%=============================================================================
clear emptyint16matrix
loadmat([mat_dir, 'test_emptyint16matrix_v6.1_GLNX86.mat']);
emptyint16matrix_ref = int16(zeros(0, 0));
assert_isequal(emptyint16matrix, emptyint16matrix_ref);
%=============================================================================
clear emptyint16matrix
loadmat([mat_dir, 'test_emptyint16matrix_v7.1_GLNX86.mat']);
assert_isequal(emptyint16matrix, emptyint16matrix_ref);
%=============================================================================
clear emptyint32matrix
loadmat([mat_dir, 'test_emptyint32matrix_v6.1_GLNX86.mat']);
emptyint32matrix_ref = int32(zeros(0, 0));
assert_isequal(emptyint32matrix, emptyint32matrix_ref);
%=============================================================================
clear emptyint32matrix
loadmat([mat_dir, 'test_emptyint32matrix_v7.1_GLNX86.mat']);
assert_isequal(emptyint32matrix, emptyint32matrix_ref);
%=============================================================================
clear emptyint8matrix
loadmat([mat_dir, 'test_emptyint8matrix_v6.1_GLNX86.mat']);
emptyint8matrix_ref = int8(zeros(0, 0));
assert_isequal(emptyint8matrix, emptyint8matrix_ref);
%=============================================================================
clear emptyint8matrix
loadmat([mat_dir, 'test_emptyint8matrix_v7.1_GLNX86.mat']);
assert_isequal(emptyint8matrix, emptyint8matrix_ref);
%=============================================================================
clear emptysparse
loadmat([mat_dir, 'test_emptysparse_v6.1_GLNX86.mat']);
assert_istrue(issparse(emptysparse))
assert_isequal(size(emptysparse),[0 0])
%=============================================================================
clear emptysparse
loadmat([mat_dir, 'test_emptysparse_v7.1_GLNX86.mat']);
assert_istrue(issparse(emptysparse))
assert_isequal(size(emptysparse),[0 0])
%=============================================================================
clear emptyuint16matrix
loadmat([mat_dir, 'test_emptyuint16matrix_v6.1_GLNX86.mat']);
emptyuint16matrix_ref = uint16(zeros(0, 0));
assert_isequal(emptyuint16matrix, emptyuint16matrix_ref);
%=============================================================================
clear emptyuint16matrix
loadmat([mat_dir, 'test_emptyuint16matrix_v7.1_GLNX86.mat']);
assert_isequal(emptyuint16matrix, emptyuint16matrix_ref);
%=============================================================================
clear emptyuint32matrix
loadmat([mat_dir, 'test_emptyuint32matrix_v6.1_GLNX86.mat']);
emptyuint32matrix_ref = uint32(zeros(0, 0));
assert_isequal(emptyuint32matrix, emptyuint32matrix_ref);
%=============================================================================
clear emptyuint32matrix
loadmat([mat_dir, 'test_emptyuint32matrix_v7.1_GLNX86.mat']);
assert_isequal(emptyuint32matrix, emptyuint32matrix_ref);
%=============================================================================
clear emptyuint8matrix
loadmat([mat_dir, 'test_emptyuint8matrix_v6.1_GLNX86.mat']);
emptyuint8matrix_ref = uint8(zeros(0, 0));
assert_isequal(emptyuint8matrix, emptyuint8matrix_ref);
%=============================================================================
clear emptyuint8matrix
loadmat([mat_dir, 'test_emptyuint8matrix_v7.1_GLNX86.mat']);
assert_isequal(emptyuint8matrix, emptyuint8matrix_ref);
%=============================================================================
