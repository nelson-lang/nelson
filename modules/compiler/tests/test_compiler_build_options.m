%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
if ~ismodule('compiler')
  addmodule(fullfile(nelsonroot(), 'modules', 'compiler'), 'compiler');
end
directory = tempname();
mkdir(directory);
cleanup = onCleanup(@() rmdir(directory, 's'));
entry = fullfile(directory, 'structured_entry.m');
filewrite(entry, 'function structured_entry(); disp(''STRUCTURED_OK''); end');
opts = nelson.compiler.BuildOptions(entry);
asserts.istrue(isa(opts, 'nelson.compiler.BuildOptions'));
expected = {'AppFile'; 'ExecutableName'; 'ExecutableIcon'; 'ExecutableVersion'; ...
  'OutputDir'; 'AdditionalFiles'; 'AutoDetectDataFiles'; 'CustomHelpTextFile'; ...
  'EmbedArchive'; 'Mode'; 'NoConsole'; 'RuntimeMode'; 'RuntimeLogFile'; 'SupportPackages'; 'TreatInputsAsNumeric'; 'Verbose'};
asserts.isequal(sort(properties(opts)), sort(expected));
asserts.isequal(sort(fieldnames(struct(opts))), sort(expected));
asserts.isequal(opts.ExecutableName, 'structured_entry');
asserts.isequal(opts.ExecutableVersion, '1.0.0.0');
asserts.isequal(opts.RuntimeMode, 'bundled');
asserts.isequal(opts.Mode, 'auto');
asserts.isfalse(opts.NoConsole);
asserts.istrue(opts.AutoDetectDataFiles);
asserts.isfalse(opts.Verbose);
asserts.isfalse(opts.TreatInputsAsNumeric);
asserts.isequal(opts.AdditionalFiles, {});
configured = ncc('options', entry, 'ExecutableName', 'renamed', ...
  'OutputDir', directory, 'RuntimeMode', 'installed', 'Mode', 'cli', ...
  'AutoDetectDataFiles', 'Off', 'Verbose', 1);
asserts.isequal(configured.ExecutableName, 'renamed');
asserts.isfalse(configured.AutoDetectDataFiles);
asserts.istrue(configured.Verbose);
snapshot = struct(configured);
restored = nelson.compiler.BuildOptions(snapshot.AppFile, rmfield(snapshot, 'AppFile'));
asserts.isequal(struct(restored), snapshot);
copy = configured;
copy.ExecutableName = 'other';
asserts.isequal(configured.ExecutableName, 'renamed');
asserts.isequal(copy.ExecutableName, 'other');
booleanNames = {'AutoDetectDataFiles', 'Verbose', 'TreatInputsAsNumeric'};
if ispc()
  booleanNames{end + 1} = 'NoConsole';
end
for name = booleanNames
  for value = {true, 1, uint8(1), 'on', "ON"}
    copy.(name{1}) = value{1};
    asserts.isequal(copy.(name{1}), true);
  end
  for value = {false, 0, int8(0), 'off', "OFF"}
    copy.(name{1}) = value{1};
    asserts.isequal(copy.(name{1}), false);
  end
  for value = {2, -1, NaN, Inf, 1i, [], [true false], 'yes', ["on" "off"], {true}}
    assert_option_rejected(copy, name{1}, value{1});
  end
end
for value = {'', '../escape', 'bad-name', 'name.exe', '1name', [char(0), 'name'], ["a" "b"]}
  assert_option_rejected(copy, 'ExecutableName', value{1});
end
if ispc()
  assert_option_rejected(copy, 'ExecutableName', 'CON');
  assert_option_rejected(copy, 'OutputDir', 'C:relative');
end
assert_option_rejected(copy, 'AppFile', fullfile(directory, 'absent.m'));
assert_option_rejected(copy, 'OutputDir', entry);
assert_option_rejected(copy, 'CustomHelpTextFile', fullfile(directory, 'absent.txt'));
assert_option_rejected(copy, 'Mode', 'other');
assert_option_rejected(copy, 'RuntimeMode', 'other');
for value = {[], 1, {{entry}}, {''}, repmat({entry}, 2, 2)}
  assert_option_rejected(copy, 'AdditionalFiles', value{1});
end
copy.AdditionalFiles = {entry; string(entry)};
expectedEntry = entry;
expectedDirectory = directory;
if ispc()
  expectedEntry = strrep(entry, '\', '/');
  expectedDirectory = strrep(directory, '\', '/');
end
asserts.isequal(copy.AdditionalFiles, {expectedEntry, expectedEntry});
copy.AdditionalFiles = '';
asserts.isequal(copy.AdditionalFiles, {});
configuredVersion = ncc('options', entry, 'ExecutableVersion', "4.0");
asserts.isequal(configuredVersion.ExecutableVersion, '4.0');
for value = {'', '1.', '.1', '1..2', '1.2.3.4.5', '65536', '-1', '1e2', '1 2', ...
  '1.a', '1/2', 'NaN', 'Inf', [char(0), '1'], ["1" "2"], 1, {"1"}, ['12'; '34']}
  assert_option_rejected(copy, 'ExecutableVersion', value{1});
end
logged = ncc('options', entry, 'RuntimeLogFile', 'app.log');
asserts.isequal(logged.RuntimeLogFile, 'app.log');
assert_rejected(@() ncc('options', entry, 'OutputDir'), 'Nelson:nelsonc:options:buildOptionsRequireNameValueOrStruct');
assert_rejected(@() ncc('options', entry, struct('unknown', true)), 'Nelson:nelsonc:options:unknownUnsupportedBuildOption');
assert_rejected(@() ncc(copy, true), 'Nelson:nelsonc:arguments:buildOptionsExplainLinkOnly');
assert_rejected(@() ncc(copy, struct()), 'Nelson:nelsonc:arguments:buildOptionsExplainLinkOnly');
assert_rejected(@() nelson.compiler.build(struct()), 'Nelson:nelsonc:options:scalarNelsonCompilerBuildOptionsObject');
assert_rejected(@() nelson.compiler.analyze(struct()), 'Nelson:nelsonc:options:scalarNelsonCompilerBuildOptionsObject');
oldDirectory = pwd();
restoreDirectory = onCleanup(@() cd(oldDirectory));
cd(directory);
fixed = ncc('options', 'structured_entry.m', 'OutputDir', 'output', 'AdditionalFiles', '*.m');
frozen = struct(fixed);
cd(oldDirectory);
asserts.isequal(struct(fixed), frozen);
asserts.isequal(fixed.AppFile, expectedEntry);
asserts.isequal(fixed.AdditionalFiles, {[expectedDirectory, '/*.m']});
asserts.isequal(fixed.OutputDir, [expectedDirectory, '/output']);
if ~ispc()
  fixed.AdditionalFiles = '\relative-name';
  asserts.isequal(fixed.AdditionalFiles, {fullfile(oldDirectory, '\relative-name')});
end
clear restoreDirectory;
clear cleanup;

function assert_option_rejected(options, name, value)
  identifier = '';
  try
    options.(name) = value;
  catch ex
    identifier = ex.identifier;
  end
  asserts.istrue(strcmp(identifier, 'Nelson:nelsonc:options') || startsWith(identifier, 'Nelson:nelsonc:options:'));
end

function assert_rejected(callback, expected)
  identifier = '';
  try
    callback();
  catch ex
    identifier = ex.identifier;
  end
  asserts.isequal(identifier, expected);
end
