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
filename = fullfile(folder, 'fclose_direct.txt');
%=============================================================================
fid = fopen(filename, 'wt');
asserts.istrue(fid > 0);
asserts.isequal(fclose(fid), 0);
asserts.isequal(fclose(fid), -1);
%=============================================================================
rmfile(filename);
rmdir(folder);
%=============================================================================
