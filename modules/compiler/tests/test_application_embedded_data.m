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
A = [1, 2, 3; 4, 5, 6];
flags = logical([1, 0, 1]);
counts = uint16([0, 65535]);
z = complex([1, 3], [2, -4]);
metadata = struct('title', 'embedded data', 'revision', 2);
savenh5(fullfile(source, 'native.nh5'), 'A', 'flags', 'counts', 'z', 'metadata');
savemat(fullfile(source, 'classic.mat'), '-v7', 'A', 'flags', 'counts', 'z', 'metadata');
savemat(fullfile(source, 'hierarchical.mat'), '-v7.3', 'A', 'flags', 'counts', 'z', 'metadata');
files = {'native.nh5', 'classic.mat', 'hierarchical.mat'};
entry = fullfile(source, 'data_entry.m');
verify_detection(entry, source, files);
write_entry(entry);
options = ncc('options', entry, 'OutputDir', fullfile(directory, 'installed'), 'RuntimeMode', 'installed');
plan = nelson.compiler.analyze(options);
assert_data_files(plan, files);
payload = fullfile(directory, 'payload');
manifest = __nelsonc_stage_application__(plan, payload, false);
for k = 1:numel(files)
  original = fullfile(source, files{k});
  extracted = fullfile(payload, 'roots', '1', files{k});
  asserts.isequal(sha256(extracted, '-file'), sha256(original, '-file'));
end
installed = ncc(options);
repeat = options;
repeat.OutputDir = fullfile(directory, 'repeat');
again = ncc(repeat);
asserts.isequal(installed.SHA256, again.SHA256);
options.RuntimeMode = 'bundled';
options.OutputDir = fullfile(directory, 'bundled');
bundled = ncc(options);
asserts.istrue(all(ismember({'matio', 'hdf5'}, bundled.RuntimePlan.modules)));
rmdir(source, 's');
rmdir(payload, 's');
relocated = fullfile(directory, 'relocated');
mkdir(relocated);
for mode = {'installed', 'bundled'}
  [ok, message] = movefile(fullfile(directory, mode{1}), relocated);
  asserts.istrue(ok, message);
end
previousRuntime = getenv('NELSONC_RUNTIME_ROOT');
restoreRuntime = onCleanup(@() setenv('NELSONC_RUNTIME_ROOT', previousRuntime));
setenv('NELSONC_RUNTIME_ROOT', nelsonroot());
[parent, stem, extension] = fileparts(installed.Executable);
for mode = {'installed', 'bundled'}
  executable = fullfile(relocated, mode{1}, [stem, extension]);
  [status, output] = system(['"', executable, '"'], 60);
  asserts.istrue(status == 0, output);
  asserts.istrue(contains(output, 'EMBEDDED_DATA_OK'), output);
end
clear restoreRuntime;
clear cleanup;

function assert_data_files(plan, names)
  asserts.istrue(plan.complete, jsonencode(plan.diagnostics));
  for k = 1:numel(names)
    asserts.istrue(any(strcmp({plan.files.relative}, names{k})), names{k});
  end
end

function verify_detection(entry, source, names)
  filewrite(entry, 'function data_entry(); disp(42); end');
  minimal = nelson.compiler.analyze(ncc('options', entry));
  asserts.isfalse(any(ismember(minimal.runtime.modules, {'matio', 'hdf5'})));
  lines = {'function data_entry()', ...
    'loadnh5(''native.nh5'');', 'loadmat classic.mat', 'load(''hierarchical.mat'');', 'end'};
  filewrite(entry, strjoin(lines, char(10)));
  plan = jsondecode(__nelsonc_plan__('link', entry));
  assert_data_files(plan, names);
  disabled = jsondecode(__nelsonc_plan__('link', entry, {}, false));
  asserts.istrue(disabled.complete);
  asserts.isfalse(any(ismember({disabled.files.relative}, names)));
  explicit = cellfun(@(name) fullfile(source, name), names, 'UniformOutput', false);
  forced = jsondecode(__nelsonc_plan__('link', entry, explicit, false));
  assert_data_files(forced, names);
  filewrite(entry, 'function data_entry(); loadnh5(''missing.nh5''); end');
  missing = jsondecode(__nelsonc_plan__('link', entry));
  asserts.isfalse(missing.complete);
  asserts.istrue(any(strcmp({missing.diagnostics.name}, 'missing.nh5')));
end

function write_entry(entry)
  lines = {'function data_entry()', ...
    'first = load(''native'');', 'second = load(''classic'');', ...
    'third = load(''hierarchical'');', ...
    'if ~isequal(first, load(''native.nh5'')) || ~isequal(second, load(''classic.mat''))', ...
    '  error(''Filename extension resolution changed the data.'');', 'end', ...
    'load hierarchical', ...
    'if ~isequal(A, third.A)', '  error(''Command-form data loading failed.'');', 'end', ...
    'for item = {first, second, third}', ...
    '  value = item{1};', ...
    '  if ~isequal(value.A, [1, 2, 3; 4, 5, 6]) || ~isequal(value.flags, logical([1, 0, 1]))', ...
    '    error(''Embedded matrix or logical data changed.'');', '  end', ...
    '  if ~isequal(value.counts, uint16([0, 65535])) || ~isequal(value.z, complex([1, 3], [2, -4]))', ...
    '    error(''Embedded integer or complex data changed.'');', '  end', ...
    '  if ~isequal(value.metadata, struct(''title'', ''embedded data'', ''revision'', 2))', ...
    '    error(''Embedded structured data changed.'');', '  end', 'end', ...
    'if ~isdeployed() || ismodule(''compiler'')', '  error(''Invalid deployment environment.'');', 'end', ...
    'disp(''EMBEDDED_DATA_OK'');', 'end'};
  filewrite(entry, strjoin(lines, char(10)));
end
