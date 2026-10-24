%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--RELEASE ONLY-->
if ~ismodule('compiler')
  addmodule(fullfile(nelsonroot(), 'modules', 'compiler'), 'compiler');
end
directory = tempname();
mkdir(directory);
cleanup = onCleanup(@() rmdir(directory, 's'));
oldRuntime = getenv('NELSONC_RUNTIME_ROOT');
restoreEnvironment = onCleanup(@() setenv('NELSONC_RUNTIME_ROOT', oldRuntime));
setenv('NELSONC_RUNTIME_ROOT', nelsonroot());
extension = '';
if ispc()
  extension = '.exe';
end
body = ['function nc_repro_entry(); ', ...
  'if ~isdeployed(); error(''Expected deployed execution.''); end; ', ...
  'if ~isfile(fullfile(ctfroot(), ''application.json'')); error(''Missing archive root.''); end; ', ...
  'disp(nc_repro_package.helper(21)); ', ...
  'disp(fileread(fullfile(fileparts(mfilename(''fullpath'')), ''data.txt''))); end'];
for mode = {'installed', 'bundled'}
  applications = {};
  for variant = 1:2
    root = fullfile(directory, [mode{1}, num2str(variant)]);
    source = fullfile(root, 'source');
    mkdir(source);
    mkdir(fullfile(source, '+nc_repro_package'));
    entry = fullfile(source, 'nc_repro_entry.m');
    helper = fullfile(source, '+nc_repro_package', 'helper.m');
    resource = fullfile(source, 'data.txt');
    if variant == 2
      filewrite(resource, 'REPRODUCIBLE_OK');
      pause(2.1);
    end
    filewrite(entry, body);
    filewrite(helper, 'function result = helper(value); result = twice(value); end; function result = twice(value); result = 2 * value; end');
    filewrite(resource, 'REPRODUCIBLE_OK');
    applications{variant} = fullfile(root, ['application', extension]);
    ncc(entry, '--cli', '--runtime', mode{1}, '-a', resource, '--help-file', resource, '-o', applications{variant});
    rmdir(source, 's');
  end
  asserts.isequal(sha256(applications{1}, '-file'), sha256(applications{2}, '-file'));
  for variant = 1:2
    [status, output] = system(['"', applications{variant}, '" 2>&1'], 60);
    asserts.isequal(status, 0, output);
    asserts.istrue(contains(output, 'REPRODUCIBLE_OK'), output);
    asserts.istrue(contains(output, '42'), output);
  end
end
clear restoreEnvironment cleanup;
