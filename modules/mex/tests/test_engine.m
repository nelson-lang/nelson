%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
if ispc() && ~havecompiler()
  configuremsvc();
end
%=============================================================================
destinationdir = [tempdir(), 'test_engine/'];
if isdir(destinationdir)
  rmdir(destinationdir, 's');
end
mkdir(destinationdir);
destination = [destinationdir, 'test_engine.c'];
status = copyfile('test_engine.c', destinationdir);
cd(destinationdir);
mex('-client', 'engine', 'test_engine.c');
if ispc()
  generated_executable = [destinationdir, 'test_engine.exe'];
else
  generated_executable = [destinationdir, 'test_engine'];
end
assert_istrue(isfile(generated_executable));
%=============================================================================
