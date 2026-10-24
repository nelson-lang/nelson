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
mkdir(source);
entry = fullfile(source, 'public_entry.m');
lines = {'function public_entry(text)', ...
  'payload = fileread(fullfile(fileparts(mfilename(''fullpath'')), ''payload.txt''));', ...
  'if ~ischar(text) || ~isdeployed() || ismodule(''compiler'')', ...
  '  error(''Invalid standalone environment.'');', 'end', ...
  'disp([''PUBLIC_BUILD_OK:'', text, '':'', payload]);', 'end'};
filewrite(entry, strjoin(lines, char(10)));
data = fullfile(source, 'payload.txt');
filewrite(data, 'payload');
help = fullfile(source, 'help.txt');
filewrite(help, 'PUBLIC_HELP');
options = compiler.build.StandaloneApplicationOptions(entry, ...
  'OutputDir', fullfile(directory, 'console'), 'ExecutableName', 'public_app', ...
  'AdditionalFiles', data, 'AutoDetectDataFiles', false, 'CustomHelpTextFile', help);
beforePath = path();
beforeDirectory = pwd();
text = evalc('result = compiler.build.standaloneApplication(options);');
asserts.isequal(text, '');
asserts.istrue(isa(result, 'compiler.build.Results'));
asserts.isequal(result.BuildType, 'standaloneApplication');
asserts.isequal(size(result.Files), [2, 1]);
asserts.isequal(result.Files{2}, fullfile(options.OutputDir, 'readme.txt'));
asserts.istrue(contains(fileread(result.Files{2}), 'NELSONC_RUNTIME_ROOT'));
asserts.istrue(isa(result.Options, 'compiler.build.StandaloneApplicationOptions'));
asserts.isequal(struct(result.Options), struct(options));
asserts.isequal(result.IncludedSupportPackages, {});
deps = result.RuntimeDependencies;
asserts.istrue(isa(deps, 'compiler.runtime.Dependencies'));
asserts.istrue(istable(deps.Required));
asserts.istrue(istable(deps.Optional));
asserts.istrue(height(deps.Required) > 0);
asserts.isequal(height(deps.Optional), 0);
asserts.isequal(deps.Required.Properties.VariableNames, {'Source', 'RelativePath', 'SHA256'});
asserts.isfalse(any(contains(deps.Required.RelativePath, 'modules/compiler/')));
snapshot = deps.__nelsonc_snapshot__();
asserts.isfalse(snapshot.gui);
asserts.isequal(height(deps.Required), numel(snapshot.files));
for k = [1, numel(snapshot.files)]
  asserts.isequal(deps.Required.SHA256{k}, sha256(deps.Required.Source{k}, '-file'));
end
assert_read_only(result, 'BuildType', 'changed');
assert_read_only(result, 'Files', {});
assert_read_only(result, 'Options', []);
assert_read_only(deps, 'Required', []);
asserts.isequal(path(), beforePath);
asserts.isequal(pwd(), beforeDirectory);
asserts.isfalse(isfolder(fullfile(options.OutputDir, 'public_app.runtime')));
originalDigest = sha256(result.Files{1}, '-file');
options.OutputDir = fullfile(directory, 'repeat');
again = compiler.build.standaloneApplication(options);
asserts.isequal(sha256(again.Files{1}, '-file'), originalDigest);
asserts.isequal(fileread(again.Files{2}), fileread(result.Files{2}));
asserts.isequal(fileread(fullfile(again.Options.OutputDir, 'buildresult.json')), ...
  fileread(fullfile(result.Options.OutputDir, 'buildresult.json')));
asserts.isfalse(strcmp(result.Options.OutputDir, options.OutputDir));
options.Verbose = true;
options.OutputDir = fullfile(directory, 'no_output');
text = evalc('compiler.build.standaloneApplication(options);');
asserts.istrue(contains(text, 'Created '));
asserts.istrue(isfile(fullfile(options.OutputDir, 'readme.txt')));
applications = {result};
if ispc()
  windowed = compiler.build.standaloneWindowsApplication(entry, ...
    'OutputDir', fullfile(directory, 'windowed'), 'AdditionalFiles', data);
  asserts.isequal(windowed.BuildType, 'standaloneWindowsApplication');
  asserts.isequal(pe_subsystem(windowed.Files{1}), 2);
  asserts.isequal(pe_subsystem(result.Files{1}), 3);
  windowedSnapshot = windowed.RuntimeDependencies.__nelsonc_snapshot__();
  asserts.isfalse(windowedSnapshot.gui);
  asserts.isequal(windowedSnapshot.modules, snapshot.modules);
  applications{end + 1} = windowed;
else
  assert_rejected(@() compiler.build.standaloneWindowsApplication(entry));
end

% A verified rebuild remains reproducible; missing dependencies publish nothing.
existing = sha256(again.Files{1}, '-file');
rebuilt = compiler.build.standaloneApplication(again.Options);
asserts.isequal(rebuilt.Files, again.Files);
asserts.isequal(sha256(again.Files{1}, '-file'), existing);
bad = fullfile(source, 'bad_entry.m');
filewrite(bad, 'function bad_entry(); missing_public_dependency(); end');
failed = fullfile(directory, 'failed');
assert_rejected(@() compiler.build.standaloneApplication(bad, 'OutputDir', failed));
asserts.isfalse(isfolder(failed));
collision = fullfile(directory, 'collision');
mkdir(collision);
filewrite(fullfile(collision, 'readme.txt'), 'USER CONTENT');
assert_rejected(@() compiler.build.standaloneApplication(entry, 'OutputDir', collision));
asserts.isequal(fileread(fullfile(collision, 'readme.txt')), 'USER CONTENT');

rmdir(source, 's');
relocated = fullfile(directory, 'relocated');
mkdir(relocated);
oldRuntime = getenv('NELSONC_RUNTIME_ROOT');
restoreRuntime = onCleanup(@() setenv('NELSONC_RUNTIME_ROOT', oldRuntime));
setenv('NELSONC_RUNTIME_ROOT', nelsonroot());
for k = 1:numel(applications)
  current = applications{k};
  destination = fullfile(relocated, int2str(k));
  [ok, message] = movefile(current.Options.OutputDir, destination);
  asserts.istrue(ok, message);
  [parent, name, extension] = fileparts(current.Files{1});
  executable = fullfile(destination, [name, extension]);
  [status, output] = system(['"', executable, '" hello'], 60);
  asserts.istrue(status == 0, output);
  asserts.istrue(contains(output, 'PUBLIC_BUILD_OK:hello:payload'), output);
end
clear restoreRuntime;
clear cleanup;

function assert_read_only(obj, name, value)
  failed = false;
  try
    obj.(name) = value;
  catch
    failed = true;
  end
  asserts.istrue(failed);
end

function assert_rejected(callback)
  failed = false;
  try
    callback();
  catch
    failed = true;
  end
  asserts.istrue(failed);
end

function value = pe_subsystem(filename)
  fid = fopen(filename, 'rb');
  closer = onCleanup(@() fclose(fid));
  fseek(fid, 60, 'bof');
  header = fread(fid, 1, 'uint32');
  fseek(fid, header + 24 + 68, 'bof');
  value = double(fread(fid, 1, 'uint16'));
end
