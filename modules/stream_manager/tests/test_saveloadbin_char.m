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
% unicode characters
A = 'NelSon 象形字';
savebin([tempdir(), 'test_saveload_char.bin'], 'A');
REF = A;
clear A;
loadbin([tempdir(), 'test_saveload_char.bin']);
assert_isequal(A, REF);
%=============================================================================
% unicode
A = ['NelSon';
'is    ';
'not   ';
'LensOn'];
savebin([tempdir(), 'test_saveload_char.bin'], 'A');
REF = A;
clear A;
loadbin([tempdir(), 'test_saveload_char.bin']);
assert_isequal(A, REF);
%=============================================================================
% nd array char
A = ones(3, 4, 6);
A = char(A);
savebin([tempdir(), 'test_saveload_char.bin'], 'A');
REF = A;
clear A;
loadbin([tempdir(), 'test_saveload_char.bin']);
assert_isequal(A, REF);
%=============================================================================
