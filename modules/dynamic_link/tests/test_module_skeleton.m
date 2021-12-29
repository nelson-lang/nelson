%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% This program is free software; you can redistribute it and/or
% modify it under the terms of the GNU Lesser General Public
% License as published by the Free Software Foundation; either
% version 2.1 of the License, or (at your option) any later version.
%
% Alternatively, you can redistribute it and/or
% modify it under the terms of the GNU General Public License as
% published by the Free Software Foundation; either version 2 of
% the License, or (at your option) any later version.
%
% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU Lesser General Public License for more details.
%
% You should have received a copy of the GNU Lesser General Public
% License along with this program. If not, see <http://www.gnu.org/licenses/>.
% LICENCE_BLOCK_END
%=============================================================================
% <--C/C++ COMPILER REQUIRED-->
% <--NO USER MODULES-->
% <--SEQUENTIAL TEST REQUIRED-->
%=============================================================================
builderFile = [nelsonroot(),'/module_skeleton/builder.m'];
if ~isfile(builderFile)
  return
end
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
