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
bodies = {'value = 42;', 'disp(''MINIMAL_SINGLE_OK'');', 'disp(sum([1, 2])); disp(''MINIMAL_MULTI_OK'');'};
markers = {'', 'MINIMAL_SINGLE_OK', 'MINIMAL_MULTI_OK'};
modes = {'bundled', 'installed'};
extension = '';
if ispc()
  extension = '.exe';
end
for k = 1:numel(bodies)
  name = sprintf('minimal_%d', k);
  entry = fullfile(directory, [name, '.m']);
  filewrite(entry, ['function ', name, '(); ', bodies{k}, ' end']);
  plan = ncc(entry, '--cli', '--explain-link');
  asserts.isequal(numel(plan.modules), k - 1);
  applications = {};
  for j = 1:numel(modes)
    application = fullfile(directory, [sprintf('a%d_%d', k, j), extension]);
    result = ncc(entry, '--cli', '--runtime', modes{j}, '-o', application);
    asserts.isequal(result, application);
    asserts.istrue(isfile(application));
    applications{j} = application;
  end
  % Neither launch can fall back to its original application source.
  rmfile(entry);
  for j = 1:numel(applications)
    [status, output] = system(['"', applications{j}, '" 2>&1'], 60);
    asserts.isequal(status, 0, output);
    if ~isempty(markers{k})
      asserts.istrue(contains(output, markers{k}), output);
    end
  end
end
clear restoreEnvironment cleanup;
