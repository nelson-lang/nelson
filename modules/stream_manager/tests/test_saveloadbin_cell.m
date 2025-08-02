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
% empty 2D cell
A = cell(3,4);
savebin([tempdir(), 'test_saveload_cell.bin'], 'A');
REF = A;
clear A;
loadbin([tempdir(), 'test_saveload_cell.bin']);
assert_isequal(A, REF);
%=============================================================================
% cell
A = {'jim', 89, [5 2 1] ; 'george', pi, 3i};
savebin([tempdir(), 'test_saveload_cell.bin'], 'A');
REF = A;
clear A;
loadbin([tempdir(), 'test_saveload_cell.bin']);
assert_isequal(A, REF);
%=============================================================================
% nd array cell
A = cell(3, 4, 2);
A{1, 1} = 3;
A{3, 4, 2} = 6;
savebin([tempdir(), 'test_saveload_cell.bin'], 'A');
REF = A;
clear A;
loadbin([tempdir(), 'test_saveload_cell.bin']);
assert_isequal(A, REF);
%=============================================================================
