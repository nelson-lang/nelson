%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Create, inspect and remove a text file in the temporary directory.
folder = [tempdir(), 'nelson-files-example/'];
if ~isdir(folder)
  mkdir(folder);
end
filename = [folder, 'measurements.txt'];
filewrite(filename, ['12.4', newline(), '13.1', newline()]);
disp(fileread(filename));
rmfile(filename);
rmdir(folder);
