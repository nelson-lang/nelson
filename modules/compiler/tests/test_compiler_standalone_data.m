%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
ncc('--help');
directory = tempname();
mkdir(directory);
cleanup = onCleanup(@() rmdir(directory, 's'));
source = fullfile(directory, 'source');
assets = fullfile(source, 'assets');
mkdir(assets);
A = [1, 2; 3, 4];
label = 'embedded standalone data';
savenh5(fullfile(assets, 'native.nh5'), 'A', 'label');
savemat(fullfile(assets, 'classic.mat'), '-v7', 'A', 'label');
savemat(fullfile(assets, 'hierarchical.mat'), '-v7.3', 'A', 'label');
files = {'native.nh5', 'classic.mat', 'hierarchical.mat'};
digests = cellfun(@(name) sha256(fullfile(assets, name), '-file'), files, 'UniformOutput', false);
entry = fullfile(source, 'standalone_data.m');
write_entry(entry, files, digests);
options = compiler.build.StandaloneApplicationOptions(entry, ...
  'AdditionalFiles', assets, 'AutoDetectDataFiles', false, ...
  'OutputDir', fullfile(directory, 'console'));
applications = {compiler.build.standaloneApplication(options)};
if ispc()
  options.OutputDir = fullfile(directory, 'windowed');
  applications{end + 1} = compiler.build.standaloneWindowsApplication(options);
end
for k = 1:numel(applications)
  snapshot = applications{k}.RuntimeDependencies.__nelsonc_snapshot__();
  asserts.istrue(all(ismember({'hdf5', 'matio'}, snapshot.modules)));
  asserts.isfalse(any(strcmp(snapshot.modules, 'compiler')));
end

% Launch only the executable, after removing every original data and source file.
rmdir(source, 's');
relocated = fullfile(directory, 'relocated');
mkdir(relocated);
caller = fullfile(directory, 'caller');
mkdir(caller);
previousDirectory = pwd();
restoreDirectory = onCleanup(@() cd(previousDirectory));
previousRuntime = getenv('NELSONC_RUNTIME_ROOT');
restoreRuntime = onCleanup(@() setenv('NELSONC_RUNTIME_ROOT', previousRuntime));
setenv('NELSONC_RUNTIME_ROOT', nelsonroot());
cd(caller);
for k = 1:numel(applications)
  executable = applications{k}.Files{1};
  [parent, name, extension] = fileparts(executable);
  destination = fullfile(relocated, [name, '_', int2str(k), extension]);
  [ok, message] = movefile(executable, destination);
  asserts.istrue(ok, message);
  for runIndex = 1:2
    [status, output] = system(['"', destination, '"'], 60);
    asserts.istrue(status == 0, output);
    asserts.istrue(contains(output, 'STANDALONE_DATA_OK'), output);
  end
end
clear restoreDirectory;
clear restoreRuntime;
clear cleanup;

function write_entry(entry, names, digests)
  lines = {'function standalone_data()', ...
    'directory = fullfile(fileparts(mfilename(''fullpath'')), ''assets'');', ...
    'if ~isdeployed() || ~startsWith(directory, ctfroot())', ...
    '  error(''Resources must be inside the deployed application.'');', 'end'};
  for k = 1:numel(names)
    lines{end + 1} = ['if ~strcmp(sha256(fullfile(directory, ''', names{k}, ...
      '''), ''-file''), ''', digests{k}, ''')'];
    lines{end + 1} = '  error(''Embedded file bytes changed.'');';
    lines{end + 1} = 'end';
  end
  lines = [lines, {'first = loadnh5(fullfile(directory, ''native.nh5''));', ...
    'second = loadmat(fullfile(directory, ''classic.mat''));', ...
    'third = load(fullfile(directory, ''hierarchical.mat''));', ...
    'expected = struct(''A'', [1, 2; 3, 4], ''label'', ''embedded standalone data'');', ...
    'if ~isequal(first, expected) || ~isequal(second, expected) || ~isequal(third, expected)', ...
    '  error(''Embedded data values changed.'');', 'end', ...
    'disp(''STANDALONE_DATA_OK'');', 'end'}];
  filewrite(entry, strjoin(lines, char(10)));
end
