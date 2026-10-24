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
entry = fullfile(directory, 'options_entry.m');
filewrite(entry, 'function options_entry(); disp(42); end');
opts = compiler.build.StandaloneApplicationOptions(entry);
expected = {'AppFile'; 'AdditionalFiles'; 'AutoDetectDataFiles'; 'CustomHelpTextFile'; ...
  'EmbedArchive'; 'ExecutableName'; 'ExecutableIcon'; 'ExecutableVersion'; 'OutputDir'; 'RuntimeLogFile'; 'SupportPackages'; 'TreatInputsAsNumeric'; 'Verbose'};
asserts.isequal(sort(properties(opts)), sort(expected));
asserts.isequal(opts.ExecutableName, 'options_entry');
asserts.isequal(opts.ExecutableVersion, '1.0.0.0');
asserts.isequal(opts.RuntimeLogFile, '');
asserts.isequal(opts.ExecutableIcon, '');
asserts.istrue(opts.EmbedArchive);
opts.EmbedArchive = 'OFF';
asserts.isfalse(opts.EmbedArchive);
asserts.istrue(endsWith(opts.OutputDir, 'options_entrystandaloneApplication'));
asserts.istrue(opts.AutoDetectDataFiles);
asserts.isfalse(opts.Verbose);
asserts.isfalse(opts.TreatInputsAsNumeric);
for value = {true, 1, uint8(1), 'on', "ON"}
  converted = compiler.build.StandaloneApplicationOptions(entry, 'TreatInputsAsNumeric', value{1});
  asserts.istrue(converted.TreatInputsAsNumeric);
  converted.TreatInputsAsNumeric = "off";
  asserts.isfalse(converted.TreatInputsAsNumeric);
end
asserts.isequal(opts.AdditionalFiles, {});
opts = compiler.build.StandaloneApplicationOptions(string(entry), 'executablename', 'renamed', ...
  "Verbose", 'on', 'AutoDetectDataFiles', 0, 'ExecutableVersion', '2.4');
asserts.isequal(opts.ExecutableName, 'renamed');
asserts.istrue(endsWith(opts.OutputDir, 'renamedstandaloneApplication'));
asserts.istrue(opts.Verbose);
asserts.isfalse(opts.AutoDetectDataFiles);
copy = opts;
opts.ExecutableName = 'later';
asserts.isequal(copy.ExecutableName, 'renamed');
values = struct(copy);
asserts.isequal(values.ExecutableVersion, '2.4');
for pair = {{'Verbose', 2}, {'AutoDetectDataFiles', NaN}, {'ExecutableName', 'bad-name'}, ...
  {'ExecutableVersion', '1.65536'}, {'RuntimeMode', 'bundled'}, {'NoConsole', true}, ...
  {'Mode', 'gui'}, {'TreatInputsAsNumeric', 2}, {'ExecutableIcon', 'icon.png'}, ...
  {'EmbedArchive', 2}, {'SupportPackages', {'none', 'package'}}, ...
  {'AppFile', entry}, {'UnknownOption', true}}
  values = pair{1};
  rejected = false;
  try
    compiler.build.StandaloneApplicationOptions(entry, values{:});
  catch ex
    rejected = (strcmp(ex.identifier, 'Nelson:nelsonc:options') || startsWith(ex.identifier, 'Nelson:nelsonc:options:'));
  end
  asserts.istrue(rejected, values{1});
end
assert_rejected(@() compiler.build.StandaloneApplicationOptions(entry, 'Verbose'));
assert_rejected(@() compiler.build.StandaloneApplicationOptions(entry, struct('Verbose', true)));
assert_rejected(@() compiler.build.standaloneApplication(opts, 'Verbose', true));
assert_rejected(@() compiler.build.standaloneApplication(ncc('options', entry)));
legacy = ncc('options', entry);
asserts.isequal(legacy.RuntimeMode, 'bundled');
clear cleanup;

function assert_rejected(callback)
  rejected = false;
  try
    callback();
  catch
    rejected = true;
  end
  asserts.istrue(rejected);
end
