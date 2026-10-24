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
filename = fullfile(folder, 'fseek_direct.txt');
filewrite(filename, 'abcdef');
%=============================================================================
fid = fopen(filename, 'rt');
asserts.isequal(fseek(fid, 2, 'bof'), 0);
asserts.isequal(fread(fid, 1, 'char'), 'c');
asserts.isequal(fseek(fid, -1, 'eof'), 0);
asserts.isequal(fread(fid, 1, 'char'), 'f');
fclose(fid);
%=============================================================================
rmfile(filename);
rmdir(folder);
%=============================================================================
