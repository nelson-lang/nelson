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
% single
A = single(eye(5,4));
savebin([tempdir(), 'test_saveload_single.bin'], 'A');
REF = A;
clear A;
loadbin([tempdir(), 'test_saveload_single.bin']);
assert_isequal(A, REF);
%=============================================================================
% complex single
A = single(eye(5,4) + 2i);
savebin([tempdir(), 'test_saveload_single.bin'], 'A');
REF = A;
clear A;
loadbin([tempdir(), 'test_saveload_single.bin']);
assert_isequal(A, REF);
%=============================================================================
% single empty
A = single(ones(0,3));
savebin([tempdir(), 'test_saveload_single.bin'], 'A');
REF = A;
clear A;
loadbin([tempdir(), 'test_saveload_single.bin']);
assert_isequal(A, REF);
%=============================================================================
