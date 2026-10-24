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
filename = fullfile(folder, 'frewind_direct.txt');
%=============================================================================
fid = fopen(filename, 'w+t');
fprintf(fid, 'abc');
frewind(fid);
fprintf(fid, 'xy');
fclose(fid);
%=============================================================================
asserts.isequal(fileread(filename), 'xyc');
rmfile(filename);
rmdir(folder);
%=============================================================================
