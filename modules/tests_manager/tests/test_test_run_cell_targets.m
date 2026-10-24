%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--CLI MODE-->
% <--SEQUENTIAL TEST REQUIRED-->
skip_if_no_native_unittest_runner();
%=============================================================================
moduleRoot = modulepath('tests_manager');
savedPath = path();
savedWorkers = maxNumCompThreads(1);
savedNative = getenv('NELSON_UNITTEST_FORCE_NATIVE_RUNNER');
setenv('NELSON_UNITTEST_FORCE_NATIVE_RUNNER', 'TRUE');
addpath(fullfile(moduleRoot, 'functions'));
addpath(fullfile(moduleRoot, 'functions', '+nelson', '+unittest', 'private'));
fixture = fullfile(tempdir(), ['cell_targets_', createGUID()]);
mkdir(fixture);
cleanup = onCleanup(@() cleanupCellTargets(fixture, savedPath, savedWorkers, savedNative));

files = cell(4, 1);
for k = 1:4
  files{k} = fullfile(fixture, ['test_target_', int2str(k), '.m']);
  filewrite(files{k}, 'asserts.istrue(true);');
end
expectedFiles = cellfun(@fullpath, files, 'UniformOutput', false);
asserts.isequal(getFilesToTest(files), expectedFiles);
asserts.isequal(getFilesToTest(files'), expectedFiles);
asserts.isequal(getFilesToTest(reshape(files, 2, 2)), expectedFiles);
asserts.isequal(getFilesToTest(files{1}), expectedFiles(1));
asserts.isequal(getFilesToTest({fullfile(fixture, 'missing.m'); files{2}}), expectedFiles(2));
asserts.istrue(isempty(getFilesToTest(cell(0, 1))));

asserts.istrue(ismodule('string'));
asserts.istrue(ismodule('tests_manager'));
missing = ['missing_', createGUID()];
moduleNames = {'string'; missing; 'tests_manager'};
expectedModules = {'string'; 'tests_manager'};
asserts.isequal(getModulesToTest(moduleNames), expectedModules);
asserts.isequal(getModulesToTest(moduleNames'), expectedModules);
asserts.isequal(getModulesToTest({missing; 'string'}), {'string'});
asserts.isequal(getModulesToTest(reshape({'string'; missing; 'tests_manager'; missing}, 2, 2)), expectedModules);

columnXml = fullfile(fixture, 'column.xml');
rowXml = fullfile(fixture, 'row.xml');
matrixXml = fullfile(fixture, 'matrix.xml');
output = evalc('status = test_run(files(1:3), columnXml);');
asserts.istrue(status);
assertReportCount(columnXml, 3);
output = evalc('status = test_run(files(1:3)'', rowXml);');
asserts.istrue(status);
assertReportCount(rowXml, 3);
output = evalc('status = test_run(reshape(files, 2, 2), matrixXml);');
asserts.istrue(status);
assertReportCount(matrixXml, 4);

filewrite(files{2}, 'asserts.istrue(false);');
failureXml = fullfile(fixture, 'failure.xml');
output = evalc('status = test_run(files(1:3), failureXml);');
asserts.isfalse(status);
assertReportCount(failureXml, 3);
asserts.istrue(contains(fileread(failureXml), '<error type="error"'));
%=============================================================================
function assertReportCount(filename, count)
  xml = fileread(filename);
  asserts.istrue(contains(xml, ['<testsuites tests="', int2str(count), '"']));
  asserts.isequal(numel(regexp(xml, '<testcase ', 'match')), count);
end
%=============================================================================
function cleanupCellTargets(fixture, savedPath, savedWorkers, savedNative)
  path(savedPath);
  maxNumCompThreads(savedWorkers);
  setenv('NELSON_UNITTEST_FORCE_NATIVE_RUNNER', savedNative);
  if isdir(fixture)
    rmdir(fixture, 's');
  end
end
