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
filename = fullfile(folder, 'fsize_direct.txt');
txt = '123456789';
filewrite(filename, txt);
%=============================================================================
fid = fopen(filename, 'rt');
asserts.isequal(fsize(fid), length(txt));
fclose(fid);
%=============================================================================
rmfile(filename);
rmdir(folder);
%=============================================================================
