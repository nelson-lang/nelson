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
filename = fullfile(folder, 'nelson_searchenv_test.txt');
fid = fopen(filename, 'wt');
fprintf(fid, 'searchenv');
fclose(fid);
%=============================================================================
envName = ['NELSON_SEARCHENV_', createGUID()];
setenv(envName, folder);
paths = searchenv('nelson_searchenv_test.txt', envName);
setenv(envName);
%=============================================================================
asserts.istrue(iscell(paths));
asserts.isequal(length(paths), 1);
asserts.isequal(strrep(paths{1}, '\', '/'), strrep(filename, '\', '/'));
%=============================================================================
rmfile(filename);
rmdir(folder);
%=============================================================================
