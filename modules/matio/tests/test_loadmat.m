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
filename = fullfile(folder, 'loadmat_direct.mat');
%=============================================================================
A = reshape(1:6, 2, 3);
B = 'loadmat direct test';
savemat(filename, 'A', 'B');
clear A B;
%=============================================================================
st = loadmat(filename);
asserts.isequal(st.A, reshape(1:6, 2, 3));
asserts.isequal(st.B, 'loadmat direct test');
%=============================================================================
st = loadmat(filename, 'A');
asserts.isequal(st.A, reshape(1:6, 2, 3));
asserts.istrue(~isfield(st, 'B'));
%=============================================================================
rmfile(filename);
rmdir(folder);
%=============================================================================
% loading a missing file reports a stable identifier
asserts.checkerror('loadmat([tempname(), ''.mat''])', _('File does not exist.'), 'Nelson:matio:fileDoesNotExist');
%=============================================================================
