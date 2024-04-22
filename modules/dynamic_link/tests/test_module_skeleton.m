%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--NO USER MODULES-->
% <--SEQUENTIAL TEST REQUIRED-->
%=============================================================================
builderFile = [nelsonroot(),'/module_skeleton/builder.m'];
skip_testsuite(~isfile(builderFile))
%=============================================================================
if ispc() && ~havecompiler()
  configuremsvc()
end
skip_testsuite(~havecompiler())
%=============================================================================
sourcedir = [nelsonroot(),'/module_skeleton'];
files = dir(sourcedir, '-s');
%=============================================================================
destinationdir = [tempdir(), 'module_skeleton'];
if isdir(destinationdir)
  rmdir(destinationdir, 's');
end
mkdir(destinationdir);
for f = files(:)'
  sourcefile = [f.folder, '/', f.name];
  if ~strcmp(f.name, 'CMakeCache.txt')
    destinationfile = replace(sourcefile, sourcedir, destinationdir);
    if isdir(sourcefile)
      mkdir(destinationfile);
    else
      copyfile(sourcefile, destinationfile);
    end
  end
end
run([destinationdir, '/builder.m']);
run([destinationdir, '/loader.m']);
%=============================================================================
assert_isequal(cpp_sum(3, 4), 7);
%=============================================================================
% unload module and dependencies
if ismodule('module_skeleton')
  removemodule('module_skeleton')
end
%=============================================================================
