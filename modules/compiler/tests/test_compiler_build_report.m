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
ncc('--help');
directory = tempname();
mkdir(directory);
cleanup = onCleanup(@() rmdir(directory, 's'));
entry = fullfile(directory, 'report_entry.m');
filewrite(entry, 'function report_entry(); disp(''REPORT_APP_OK''); end');
first = compiler.build.standaloneApplication(entry, 'OutputDir', fullfile(directory, 'first'));
filename = fullfile(first.Options.OutputDir, 'buildresult.json');
asserts.istrue(isfile(filename));
deep = [repmat('[', 1, 128), '0', repmat(']', 1, 128)];
assert_rejected(@() __nelsonc_plan__('build-report', deep));
asserts.isequal(numel(first.Files), 2);
asserts.isfalse(any(contains(first.Files, 'buildresult.json')));
document = jsondecode(fileread(filename));
asserts.isequal(document.format, 2);
asserts.isequal(document.launcher.format, 2);
asserts.isequal(document.launcher.kind, 'console');
expectedDiscoveryVersion = 2;
if ~ispc(); expectedDiscoveryVersion = 3; end
asserts.isequal(document.launcher.runtimeDiscoveryVersion, expectedDiscoveryVersion);
asserts.isequal(document.launcher, jsondecode(__nelsonc_executable__('launcher-contract', first.Files{1})));
asserts.isequal(document.kind, 'nelson-standalone-build');
asserts.isequal(document.applicationName, 'report_entry');
asserts.isequal(document.architecture, computer('arch'));
asserts.isfalse(isfield(document, 'options'));
asserts.isequal(numel(document.files), 2);
internalReport = first.__nelsonc_report__();
asserts.isequal(jsonencode(document), jsonencode(internalReport.Distribution));
for k = 1:numel(document.files)
  item = document.files(k);
  asserts.isequal(item.sha256, sha256(fullfile(first.Options.OutputDir, item.relative), '-file'));
end
plan = __nelsonc_distribution_plan__(first);
asserts.isequal(plan.kind, 'nelson-runtime-distribution');
asserts.isequal(plan.applications, {'report_entry'});
asserts.isequal(jsonencode(plan), jsonencode(__nelsonc_distribution_plan__(filename)));
asserts.isequal(jsonencode(plan), jsonencode(__nelsonc_distribution_plan__(string(filename))));
asserts.isequal(jsonencode(plan), jsonencode(__nelsonc_distribution_plan__({string(filename)})));
asserts.isequal(jsonencode(plan), jsonencode(__nelsonc_distribution_plan__([first, first])));
asserts.istrue(numel(plan.runtime.startupOrder) >= numel(plan.runtime.moduleOrder));
asserts.isfalse(any(strcmp(plan.runtime.modules, 'compiler')));
for input = {[], {}, struct(), 1, '', char(0), directory, repmat({filename}, 2, 2)}
  assert_rejected(@() __nelsonc_distribution_plan__(input{1}));
end
bad = document;
bad.format = 99;
assert_rejected(@() normalize(bad));
bad.format = 1;
assert_rejected(@() normalize(bad));
assert_rejected(@() normalize(rmfield(document, 'launcher')));
for field = {'format', 'startupInterface', 'archiveMinVersion', 'archiveMaxVersion', 'runtimeDiscoveryVersion'}
  for value = {-1, 0, 99, 1.5, '1', true, 2 ^ 32}
    bad = document;
    bad.launcher.(field{1}) = value{1};
    assert_rejected(@() normalize(bad));
  end
end
bad = document;
bad.launcher.kind = 'windowed';
assert_rejected(@() normalize(bad));
bad = document;
bad.launcher.unknown = 1;
assert_rejected(@() normalize(bad));
bad = document;
bad.files(1).relative = 'buildresult.json';
assert_rejected(@() normalize(bad));
for name = {'../escape', '/absolute', 'C:/absolute', 'x\\y', 'NUL.txt', 'trailing/', 'trailing.'}
  bad = document;
  bad.runtime.files(1).relative = name{1};
  assert_rejected(@() normalize(bad), name{1});
end
% Send the escaped control character directly to the report parser.
bad = document;
bad.runtime.files(1).relative = 'CONTROL_MARKER';
encoded = strrep(jsonencode(bad), 'CONTROL_MARKER', 'x\u0000y');
assert_rejected(@() __nelsonc_plan__('build-report', encoded));
bad = document;
bad.runtime.files(1).relative = 'modules/modules.m/child';
assert_rejected(@() normalize(bad));
bad = document;
bad.runtime.directories{end + 1} = bad.runtime.files(1).relative;
assert_rejected(@() normalize(bad));
bad = document;
bad.runtime.modules{end + 1} = 'compiler';
assert_rejected(@() normalize(bad));
bad = document;
bad.runtime.bootstrapFiles = 42;
assert_rejected(@() normalize(bad));
bad = document;
bad.engineFingerprint = repmat('0', 1, 64);
assert_rejected(@() normalize(bad));
bad = document;
bad.runtime.files(1).source = 'relative-source';
assert_rejected(@() normalize(bad));
bad = document;
bad.runtime.modules(strcmp(bad.runtime.modules, 'core')) = [];
assert_rejected(@() normalize(bad));
bad = document;
engine = endsWith({bad.runtime.files.relative}, ['libnlsEngine', getdynlibext()]);
bad.runtime.files(engine) = [];
assert_rejected(@() normalize(bad));
bad = document;
bad.files(1).sha256 = repmat('Z', 1, 64);
assert_rejected(@() normalize(bad));
assert_rejected(@() __nelsonc_plan__('distribution', '[]', true));
assert_rejected(@() __nelsonc_plan__('distribution', jsonencode(repmat({struct()}, 1, 65)), false));
assert_rejected(@() __nelsonc_plan__('distribution', jsonencode({document}), 1));
check_merges(document, directory);
% A pre-existing metadata report is protected before any output is published.
collision = fullfile(directory, 'collision');
mkdir(collision);
filewrite(fullfile(collision, 'buildresult.json'), 'USER REPORT');
assert_rejected(@() compiler.build.standaloneApplication(entry, 'OutputDir', collision));
asserts.isequal(fileread(fullfile(collision, 'buildresult.json')), 'USER REPORT');
asserts.isfalse(isfile(fullfile(collision, document.executable)));
rmfile(entry);
asserts.isequal(jsonencode(plan), jsonencode(__nelsonc_distribution_plan__(filename)));
clear cleanup;

function normalized = normalize(document)
  normalized = jsondecode(__nelsonc_plan__('build-report', jsonencode(document)));
end

function plan = merge(documents, verify)
  plan = jsondecode(__nelsonc_plan__('distribution', jsonencode(documents), verify));
end

function check_merges(document, directory)
  source = fullfile(directory, 'extra.txt');
  filewrite(source, 'EXTRA');
  item = struct('source', source, 'relative', 'resources/report_extra.txt', 'sha256', sha256(source, '-file'));
  extra = document;
  extra.applicationName = 'second_application';
  extra.runtime.files(end + 1) = item;
  normalized = normalize(extra);
  both = merge({document, extra}, true);
  reverse = merge({extra, document}, true);
  asserts.isequal(jsonencode(both), jsonencode(reverse));
  asserts.isequal(numel(both.runtime.files), numel(document.runtime.files) + 1);
  ignored = extra;
  ignored.runtime.unrecognizedField = 'Not retained in a normalized runtime.';
  asserts.isequal(jsonencode(both), jsonencode(merge({document, ignored}, false)));
  if ispc()
    duplicate = extra;
    duplicate.runtime.files(end + 1) = item;
    duplicate.runtime.files(end).relative = upper(item.relative);
    assert_rejected(@() normalize(duplicate));
  end
  duplicate = extra;
  duplicate.runtime.files(end).sha256 = repmat('0', 1, 64);
  assert_rejected(@() merge({extra, duplicate}, false));
  conflicting = extra;
  conflicting.runtime.startupOrder{end + 1} = 'unused_runtime_order_entry';
  assert_rejected(@() merge({document, conflicting}, false));
  conflicting = extra;
  conflicting.runtimeVersion = 'different-runtime';
  assert_rejected(@() merge({document, conflicting}, false));
  filewrite(source, 'CHANGED');
  assert_rejected(@() merge({extra}, true));
  unverified = merge({extra}, false);
  asserts.isequal(unverified.kind, 'nelson-runtime-distribution');
  rmfile(source);
  assert_rejected(@() merge({extra}, true));
end

function assert_rejected(callback, label)
  if nargin() < 2
    label = 'Expected a rejected distribution request.';
  end
  failed = false;
  try
    callback();
  catch ex
    failed = true;
    asserts.istrue(startsWith(ex.identifier, 'Nelson:nelsonc:'), ex.message);
  end
  asserts.istrue(failed, label);
end
