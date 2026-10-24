%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% which locates a data file (name carrying an extension) on the search path.
% A name without an extension is only a function/variable lookup.
%=============================================================================
folder = tempname();
mkdir(folder);
subfolder = fullfile(folder, 'sub');
mkdir(subfolder);
%=============================================================================
A = 1;
save(fullfile(subfolder, 'which_data.mat'), 'A');
save(fullfile(subfolder, 'which_native'), 'A'); % which_native.nh5 (native)
fid = fopen(fullfile(subfolder, 'which_notes.txt'), 'wt');
fprintf(fid, 'hello');
fclose(fid);
addpath(subfolder);
%=============================================================================
otherfolder = tempname();
mkdir(otherfolder);
previousDir = pwd();
cd(otherfolder);
%=============================================================================
% a .mat on the path is located
location = which('which_data.mat');
asserts.istrue(ischar(location));
asserts.istrue(~isempty(location));
asserts.istrue(endsWith(location, 'which_data.mat'));
asserts.istrue(isfile(location));
%=============================================================================
% any extension is located, not only .mat
location = which('which_notes.txt');
asserts.istrue(~isempty(location));
asserts.istrue(endsWith(location, 'which_notes.txt'));
%=============================================================================
% no extension is NOT resolved to a data file (no extension is appended)
asserts.isequal(which('which_data'), '');
asserts.isequal(which('which_native'), '');
%=============================================================================
% a missing file returns empty
asserts.isequal(which('this_data_file_is_missing.mat'), '');
%=============================================================================
% -all returns the match in a cell
allLocations = which('which_data.mat', '-all');
asserts.istrue(iscell(allLocations));
asserts.isequal(numel(allLocations), 1);
asserts.istrue(endsWith(allLocations{1}, 'which_data.mat'));
%=============================================================================
% function lookup is unaffected
asserts.istrue(~isempty(which('sin')));
%=============================================================================
cd(previousDir);
rmpath(subfolder);
rmdir(folder, 's');
rmdir(otherfolder, 's');
%=============================================================================
