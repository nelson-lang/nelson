%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% more tests in hdf5 and matio modules
%=============================================================================
test_load_mat = [tempname(), '.mat'];
test_load_nh5 = [tempname(), '.nh5'];
%=============================================================================
A = ones(3, 4);
B = 'hello for open mat users';
save(test_load_mat, 'A', 'B');
asserts.istrue(ismatfile(test_load_mat));
clear A B;
st = load(test_load_mat);
ref.A = ones(3, 4);
ref.B = 'hello for open mat users';
asserts.isequal(st, ref);
%=============================================================================
A = ones(3, 4);
B = 'hello for open mat users';
save(test_load_nh5, 'A', 'B');
asserts.istrue(isnh5file(test_load_nh5));
clear A B;
st = load(test_load_nh5);
ref.A = ones(3, 4);
ref.B = 'hello for open mat users';
asserts.isequal(st, ref);
%=============================================================================
A = ones(3, 4);
B = 'hello for open mat users';
save(test_load_nh5, 'A', 'B', '-mat');
asserts.istrue(ismatfile(test_load_nh5));
clear A B;
st = load(test_load_nh5);
ref.A = ones(3, 4);
ref.B = 'hello for open mat users';
asserts.isequal(st, ref);
%=============================================================================
A = ones(3, 4);
B = 'hello for open mat users';
save(test_load_mat, 'A', 'B', '-nh5');
asserts.istrue(isnh5file(test_load_mat));
clear A B;
st = load(test_load_mat);
ref.A = ones(3, 4);
ref.B = 'hello for open mat users';
asserts.isequal(st, ref);
rmfile(test_load_mat);
rmfile(test_load_nh5);
%=============================================================================
