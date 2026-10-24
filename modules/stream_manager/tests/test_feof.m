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
filename = fullfile(folder, 'feof_direct.txt');
filewrite(filename, 'abc');
%=============================================================================
fid = fopen(filename, 'rt');
asserts.isequal(feof(fid), 0);
fread(fid, 3, 'char');
fread(fid, 1, 'char');
asserts.isequal(feof(fid), 1);
fclose(fid);
%=============================================================================
rmfile(filename);
rmdir(folder);
%=============================================================================
