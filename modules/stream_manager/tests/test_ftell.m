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
filename = fullfile(folder, 'ftell_direct.txt');
filewrite(filename, 'abcdef');
%=============================================================================
fid = fopen(filename, 'rt');
asserts.isequal(ftell(fid), 0);
fread(fid, 2, 'char');
asserts.isequal(ftell(fid), 2);
fseek(fid, 0, 'eof');
asserts.isequal(ftell(fid), 6);
fclose(fid);
%=============================================================================
rmfile(filename);
rmdir(folder);
%=============================================================================
