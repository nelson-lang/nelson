%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
A = 1:10;
test_dir = tempname();
mkdir(test_dir);
filename = fullfile(test_dir, '漢字.mat');
save(filename, 'A');
asserts.istrue(isfile(filename));
clear A
asserts.isfalse(isvar('A'));
load(filename);
asserts.istrue(isvar('A'));
asserts.isequal(A, 1:10);
rmdir(test_dir, 's');
%=============================================================================