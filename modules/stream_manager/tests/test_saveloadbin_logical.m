%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
addpath([fileparts(nfilename('fullpathext'), 'path'), '/loadsavebin']);
%=============================================================================
% logical
A = logical(eye(5, 4));
savebin([tempdir(), 'test_saveload_logical.bin'], 'A');
REF = A;
clear A;
loadbin([tempdir(), 'test_saveload_logical.bin']);
assert_isequal(A, REF);
%=============================================================================
% sparse logical
A = sparse(logical(eye(3,3)));
savebin([tempdir(), 'test_saveload_logical.bin'], 'A');
REF = A;
clear A;
loadbin([tempdir(), 'test_saveload_logical.bin']);
assert_isequal(A, REF);
%=============================================================================
% nd array logical
A = ones(3, 4, 6);
A = logical(A);
savebin([tempdir(), 'test_saveload_logical.bin'], 'A');
REF = A;
clear A;
loadbin([tempdir(), 'test_saveload_logical.bin']);
assert_isequal(A, REF);
%=============================================================================
