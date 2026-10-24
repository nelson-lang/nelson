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
entry = fullfile(directory, 'result_entry.m');
resource = fullfile(directory, 'data.txt');
help = fullfile(directory, 'help.txt');
filewrite(entry, 'function result_entry(); disp(''RESULT_ENTRY_OK''); end');
filewrite(resource, 'RESOURCE');
filewrite(help, 'RESULT_HELP_OK');
opts = ncc('options', entry, 'ExecutableName', 'result_app', ...
  'OutputDir', fullfile(directory, 'output'), 'AdditionalFiles', resource, ...
  'AutoDetectDataFiles', false, 'CustomHelpTextFile', help, 'RuntimeMode', 'installed', 'Mode', 'cli');
beforePath = path();
beforeDirectory = pwd();
plan = nelson.compiler.analyze(opts);
asserts.isequal(ncc(opts, '--explain-link'), plan);
asserts.isfalse(isfolder(opts.OutputDir));
asserts.istrue(plan.complete);
asserts.isequal(plan.helpText, 'RESULT_HELP_OK');
outputText = evalc('result = ncc(opts);');
asserts.isequal(outputText, '');
asserts.istrue(isa(result, 'nelson.compiler.BuildResult'));
asserts.isequal(result.BuildType, 'standaloneApplication');
asserts.isequal(result.Files, {result.Executable});
asserts.istrue(isfile(result.Executable));
asserts.isequal(result.RuntimeDirectory, '');
asserts.isequal(result.RuntimePlan, plan.runtime);
asserts.isequal(result.DependencyPlan.files, plan.files);
asserts.isequal(result.DependencyPlan.edges, plan.edges);
asserts.isequal(result.Manifest.helpText, 'RESULT_HELP_OK');
asserts.isequal(result.SHA256, sha256(result.Executable, '-file'));
asserts.isequal(struct(result.Options), struct(opts));
serialized = jsondecode(jsonencode(struct(result)));
asserts.isequal(serialized.SHA256, result.SHA256);
asserts.isequal(serialized.Options.ExecutableName, 'result_app');
report = struct(result);
report.RuntimeDirectory = fullfile(directory, 'recorded.runtime');
metadata = nelson.compiler.BuildResult(report, opts);
asserts.isequal(metadata.Files, {result.Executable, report.RuntimeDirectory});
asserts.isequal(metadata.RuntimeDirectory, report.RuntimeDirectory);
asserts.isequal(struct(metadata.Options), struct(opts));
rejected = false;
try
  nelson.compiler.BuildResult(rmfield(report, 'SHA256'), opts);
catch ex
  rejected = strcmp(ex.identifier, 'Nelson:nelsonc:result:invalidBuildReportOptions');
end
asserts.istrue(rejected);
asserts.isfalse(contains(jsonencode(result.Manifest), directory));
asserts.isfalse(contains(jsonencode(result.Manifest), strrep(directory, '\', '/')));
asserts.isequal(path(), beforePath);
asserts.isequal(pwd(), beforeDirectory);
assert_read_only(result, 'Executable', 'changed');
assert_read_only(result, 'Options', opts);
opts.ExecutableName = 'new_name';
asserts.isequal(result.Options.ExecutableName, 'result_app');
opts.ExecutableName = 'result_app';
opts.OutputDir = fullfile(directory, 'repeat');
repeat = nelson.compiler.build(opts);
asserts.isequal(result.SHA256, repeat.SHA256);
asserts.isequal(struct(result.Options), serialized.Options);
extension = '';
if ispc()
  extension = '.exe';
end
legacyOutput = fullfile(directory, 'legacy', ['result_app', extension]);
legacy = ncc(entry, '-o', legacyOutput, '--runtime', 'installed', '--cli', '-X', '-a', resource, '--help-file', help);
asserts.istrue(ischar(legacy));
asserts.isequal(sha256(legacy, '-file'), result.SHA256);
rejected = false;
try
  nelson.compiler.build(opts);
catch ex
  rejected = strcmp(ex.identifier, 'Nelson:nelsonc:output:outputExecutableRuntimeNotAlready');
end
asserts.istrue(rejected);
asserts.isequal(sha256(repeat.Executable, '-file'), result.SHA256);
opts.ExecutableName = 'without_output';
text = evalc('ncc(opts);');
asserts.isequal(text, '');
asserts.istrue(isfile(fullfile(opts.OutputDir, ['without_output', extension])));
opts.ExecutableName = 'verbose';
opts.Verbose = true;
text = evalc('nelson.compiler.build(opts);');
asserts.istrue(contains(text, 'Created '));
asserts.istrue(isfile(fullfile(opts.OutputDir, ['verbose', extension])));
filewrite(entry, 'function result_entry(); missing_structured_dependency(); end');
opts.OutputDir = fullfile(directory, 'failed');
failedPlan = nelson.compiler.analyze(opts);
asserts.isfalse(failedPlan.complete);
asserts.isfalse(isfolder(opts.OutputDir));
rejected = false;
try
  ncc(opts);
catch ex
  rejected = strcmp(ex.identifier, 'Nelson:nelsonc:dependencies');
end
asserts.istrue(rejected);
asserts.isfalse(isfolder(opts.OutputDir));
asserts.istrue(result.DependencyPlan.complete);
asserts.isequal(result.SHA256, sha256(result.Executable, '-file'));
clear cleanup;

function assert_read_only(result, name, value)
  rejected = false;
  try
    result.(name) = value;
  catch
    rejected = true;
  end
  asserts.istrue(rejected);
end
