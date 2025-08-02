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
A = struct('name', {'Pierre', 'Anna', 'Roberta'}, 'age', {43, 21, 31});
savebin([tempdir(), 'test_saveload_struct.bin'], 'A');
REF = A;
clear A;
loadbin([tempdir(), 'test_saveload_struct.bin']);
assert_isequal(A, REF);
%=============================================================================
A = struct();
savebin([tempdir(), 'test_saveload_struct.bin'], 'A');
REF = A;
clear A;
loadbin([tempdir(), 'test_saveload_struct.bin']);
assert_isequal(A, REF);
%=============================================================================
A = struct([]);
savebin([tempdir(), 'test_saveload_struct.bin'], 'A');
REF = A;
clear A;
loadbin([tempdir(), 'test_saveload_struct.bin']);
assert_isequal(A, REF);
%=============================================================================
A = struct(ones(1,0));
savebin([tempdir(), 'test_saveload_struct.bin'], 'A');
REF = A;
clear A;
loadbin([tempdir(), 'test_saveload_struct.bin']);
assert_isequal(A, REF);
%=============================================================================
A = struct(ones(0,1));
savebin([tempdir(), 'test_saveload_struct.bin'], 'A');
REF = A;
clear A;
loadbin([tempdir(), 'test_saveload_struct.bin'])
assert_isequal(A, REF);
%=============================================================================
A = struct(ones(3, 0, 2));
savebin([tempdir(), 'test_saveload_struct.bin'], 'A');
REF = A;
clear A;
loadbin([tempdir(), 'test_saveload_struct.bin']);
assert_isequal(A, REF);
%=============================================================================
c = {'tree', 37.4, 'birch'};
f = {'category','height','name'};
A = cell2struct(c, f, 2);
savebin([tempdir(), 'test_saveload_struct.bin'], 'A');
REF = A;
clear A;
loadbin([tempdir(), 'test_saveload_struct.bin']);
assert_isequal(A, REF);
%=============================================================================
c = {'birch','betula',65;  'maple','acer',50};
f = {'name', 'genus', 'height'};
A = cell2struct(c, f, 2);
savebin([tempdir(), 'test_saveload_struct.bin'], 'A');
REF = A;
clear A;
loadbin([tempdir(), 'test_saveload_struct.bin']);
assert_isequal(A, REF);
%=============================================================================
my_cell_array = {'Jimmy', 'Timothy', 'Charles'};
A = cell2struct(cell(size(my_cell_array)), my_cell_array, 2);
savebin([tempdir(), 'test_saveload_struct.bin'], 'A');
REF = A;
clear A;
loadbin([tempdir(), 'test_saveload_struct.bin']);
assert_isequal(A, REF);
%=============================================================================
clear C;
C{1}='tim';
C{2}='love';
C{3}=1.73;
A=cell2struct(C,{'firstname','familyname','height'},2);
savebin([tempdir(), 'test_saveload_struct.bin'], 'A');
REF = A;
clear A;
loadbin([tempdir(), 'test_saveload_struct.bin']);
assert_isequal(A, REF);
%=============================================================================
clear C;
C{1}='tim';
C{2}='love';
C{3}=1.73;
A=cell2struct(C,{'firstname'});
savebin([tempdir(), 'test_saveload_struct.bin'], 'A');
REF = A;
clear A;
loadbin([tempdir(), 'test_saveload_struct.bin']);
assert_isequal(A, REF);
%=============================================================================
