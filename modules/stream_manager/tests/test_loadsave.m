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
A = ones(3, 4);
B = 'hello for open mat users';
save([tempdir(), 'test_load.mat'], 'A', 'B');
assert_istrue(ismatfile([tempdir(), 'test_load.mat']));
clear A B;
st = load([tempdir(), 'test_load.mat']);
ref.A = ones(3, 4);
ref.B = 'hello for open mat users';
assert_isequal(st, ref);
%=============================================================================
A = ones(3, 4);
B = 'hello for open mat users';
save([tempdir(), 'test_load.nh5'], 'A', 'B');
assert_istrue(isnh5file([tempdir(), 'test_load.nh5']));
clear A B;
st = load([tempdir(), 'test_load.nh5']);
ref.A = ones(3, 4);
ref.B = 'hello for open mat users';
assert_isequal(st, ref);
%=============================================================================
A = ones(3, 4);
B = 'hello for open mat users';
save([tempdir(), 'test_load.nh5'], 'A', 'B', '-mat');
assert_istrue(ismatfile([tempdir(), 'test_load.nh5']));
clear A B;
st = load([tempdir(), 'test_load.nh5']);
ref.A = ones(3, 4);
ref.B = 'hello for open mat users';
assert_isequal(st, ref);
%=============================================================================
A = ones(3, 4);
B = 'hello for open mat users';
save([tempdir(), 'test_load.mat'], 'A', 'B', '-nh5');
assert_istrue(isnh5file([tempdir(), 'test_load.mat']));
clear A B;
st = load([tempdir(), 'test_load.mat']);
ref.A = ones(3, 4);
ref.B = 'hello for open mat users';
assert_isequal(st, ref);
%=============================================================================
