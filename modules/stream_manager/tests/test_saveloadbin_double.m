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
% double
A = eye(5, 4);
savebin([tempdir(), 'test_saveload_double.bin'], 'A');
REF = A;
clear A;
loadbin([tempdir(), 'test_saveload_double.bin']);
assert_isequal(A, REF);
%=============================================================================
% double complex
A = eye(5,4) + 2i;
savebin([tempdir(), 'test_saveload_double.bin'], 'A');
REF = A;
clear A;
loadbin([tempdir(), 'test_saveload_double.bin']);
assert_isequal(A, REF);
%=============================================================================
% double empty
A = ones(0,3);
savebin([tempdir(), 'test_saveload_double.bin'], 'A');
REF = A;
clear A;
loadbin([tempdir(), 'test_saveload_double.bin']);
assert_isequal(A, REF);
%=============================================================================
% nd array double
A = ones(3,4,2);
savebin([tempdir(), 'test_saveload_double.bin'], 'A');
REF = A;
clear A;
loadbin([tempdir(), 'test_saveload_double.bin']);
assert_isequal(A, REF);
%=============================================================================
% nd array double complex
A = complex(ones(3,4,2), 2);
savebin([tempdir(), 'test_saveload_double.bin'], 'A');
REF = A;
clear A;
loadbin([tempdir(), 'test_saveload_double.bin']);
assert_isequal(A, REF);
%=============================================================================
