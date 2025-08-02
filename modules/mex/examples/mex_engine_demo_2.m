%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
destinationdir = [tempdir(), 'mex_engine_demo_2/'];
if isdir(destinationdir)
  rmdir(destinationdir, 's');
end
mkdir(destinationdir);
destination = [destinationdir, 'mex_engine_demo_2.c'];
status = copyfile('mex_engine_demo_2.c', destinationdir);
cd(destinationdir);
mex('-client', 'engine', 'mex_engine_demo_2.c');
generated_executable = [destinationdir, 'mex_engine_demo_2'];
disp('Open an Terminal')
disp('Set environment variables to find Nelson (see doc mex).')
disp('Go to:')
disp(destinationdir)
disp('Launch')
disp('mex_engine_demo_2')
%=============================================================================
