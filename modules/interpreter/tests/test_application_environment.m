%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
if iswasm()
  skip_testsuite('bytecode artifact engine is not available in the portable WebAssembly runtime.');
end
%=============================================================================
asserts.isequal(nargin('__nelsonc_environment__'), 2);
asserts.isequal(nargout('__nelsonc_environment__'), 2);
originalDirectory = pwd();
originalPath = path();
directory = tempname();
mkdir(directory);
[originalSearch, originalUserPath] = __nelsonc_environment__(true, '');
cleanup = onCleanup(@() restore(directory, originalDirectory, originalPath, originalSearch, originalUserPath));
packageDirectory = fullfile(directory, 'package');
currentDirectory = fullfile(directory, 'current');
otherDirectory = fullfile(directory, 'other');
userDirectory = fullfile(directory, 'user');
write_probe(packageDirectory, 42);
write_probe(currentDirectory, 1);
write_probe(otherDirectory, 2);
write_probe(userDirectory, 3);
source = fullfile(packageDirectory, 'nc_environment_probe.m');
__bytecode_artifact__('compile', source, fullfile(packageDirectory, 'nc_environment_probe.nbc'));
delete(source);
preferencesFile = fullfile(prefdir(), 'userpath.conf');
preferencesExisted = isfile(preferencesFile);
if preferencesExisted
  preferencesHash = sha256(preferencesFile, '-file');
end
cd(currentDirectory);
asserts.isequal(feval('nc_environment_probe'), 1);
[previousSearch, previousUserPath] = __nelsonc_environment__(false, '');
asserts.istrue(previousSearch);
asserts.istrue(isempty(previousUserPath));
asserts.istrue(isempty(which('nc_environment_probe')));
addpath(packageDirectory);
asserts.isequal(feval('nc_environment_probe'), 42);
activeSource = fullfile(packageDirectory, 'nc_environment_active.m');
fid = fopen(activeSource, 'wt');
asserts.istrue(fid >= 0);
fprintf(fid, 'function value = nc_environment_active()\n__nelsonc_environment__(false, '''');\nvalue = 19;\nend\n');
fclose(fid);
rehash();
asserts.isequal(feval('nc_environment_active'), 19);
cd(otherDirectory);
rehash();
asserts.isequal(feval('nc_environment_probe'), 42);
asserts.istrue(isempty(userpath()));
rmpath(packageDirectory);
__nelsonc_environment__(false, '');
asserts.istrue(isempty(which('nc_environment_probe')));
addpath(otherDirectory);
asserts.isequal(feval('nc_environment_probe'), 2);
rmpath(otherDirectory);
__nelsonc_environment__(false, userDirectory);
asserts.isequal(feval('nc_environment_probe'), 3);
__nelsonc_environment__(true, previousUserPath);
asserts.isequal(feval('nc_environment_probe'), 2);
cd(directory);
__nelsonc_environment__(true, userDirectory);
asserts.isequal(feval('nc_environment_probe'), 3);
asserts.isequal(isfile(preferencesFile), preferencesExisted);
if preferencesExisted
  asserts.isequal(sha256(preferencesFile, '-file'), preferencesHash);
end
clear cleanup;
asserts.isequal(pwd(), originalDirectory);

function write_probe(directory, value)
  mkdir(directory);
  fid = fopen(fullfile(directory, 'nc_environment_probe.m'), 'wt');
  asserts.istrue(fid >= 0);
  fprintf(fid, 'function value = nc_environment_probe()\nvalue = %d;\nend\n', value);
  fclose(fid);
end

function restore(directory, originalDirectory, originalPath, originalSearch, originalUserPath)
  cd(originalDirectory);
  path(originalPath);
  __nelsonc_environment__(originalSearch, originalUserPath);
  rmdir(directory, 's');
end
