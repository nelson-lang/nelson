%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
folder = tempname();
mkdir(folder);
filename = fullfile(folder, 'savemat_direct.mat');
%=============================================================================
A = [1 2 3; 4 5 6];
B = true(1, 3);
savemat(filename, 'A', 'B');
%=============================================================================
asserts.istrue(ismatfile(filename));
st = loadmat(filename);
asserts.isequal(st.A, A);
asserts.isequal(st.B, B);
%=============================================================================
rmfile(filename);
rmdir(folder);
%=============================================================================
