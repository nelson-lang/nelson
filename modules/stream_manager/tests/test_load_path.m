%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% load resolves a bare filename against the search path,
% not only against the current directory.
%=============================================================================
folder = tempname();
mkdir(folder);
subfolder = fullfile(folder, 'sub');
mkdir(subfolder);
%=============================================================================
A = magic(4);
B = 'load on path test';
save(fullfile(subfolder, 'data_on_path.mat'), 'A', 'B');
save(fullfile(subfolder, 'data_on_path_native'), 'A', 'B'); % .nh5 native default
addpath(subfolder);
%=============================================================================
% move to an unrelated current directory that does not contain the files
otherfolder = tempname();
mkdir(otherfolder);
previousDir = pwd();
cd(otherfolder);
%=============================================================================
% .mat explicit extension resolved on the path
clear A B;
st = load('data_on_path.mat');
asserts.isequal(st.A, magic(4));
asserts.isequal(st.B, 'load on path test');
%=============================================================================
% no extension resolves the .mat file on the path
clear A B;
st = load('data_on_path');
asserts.isequal(st.A, magic(4));
%=============================================================================
% no extension resolves the native .nh5 file on the path
clear A B;
st = load('data_on_path_native');
asserts.isequal(st.A, magic(4));
%=============================================================================
% assign into caller (no output argument) still works from the path
clear A B;
load('data_on_path.mat', 'A');
asserts.isequal(A, magic(4));
%=============================================================================
% absolute path is unchanged
clear A B;
st = load(fullfile(subfolder, 'data_on_path.mat'));
asserts.isequal(st.A, magic(4));
%=============================================================================
% a name that exists nowhere still errors
asserts.checkerror('load(''this_file_does_not_exist_anywhere.mat'')', ...
  _('File does not exist.'));
%=============================================================================
cd(previousDir);
rmpath(subfolder);
rmdir(folder, 's');
rmdir(otherfolder, 's');
%=============================================================================
