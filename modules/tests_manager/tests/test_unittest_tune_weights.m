%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
removeCommand = @(filename)['if isfile(''', filename, '''), ', ...
  'rmfile(''', filename, '''), end'];
base = [[tempdir(), ''], 'test_unittest_tune_weights_', createGUID()];
fastFile = [base, '_test_fast.m'];
mediumFile = [base, '_test_medium.m'];
slowFile = [base, '_test_slow.m'];
verySlowFile = [base, '_test_very_slow.m'];
failedFile = [base, '_test_failed.m'];
benchFastFile = [base, '_bench_fast.m'];
benchMediumFile = [base, '_bench_medium.m'];
benchSlowFile = [base, '_bench_slow.m'];
targetFile = [base, '_test_target.m'];
files = {fastFile, mediumFile, slowFile, verySlowFile, failedFile, ...
  benchFastFile, benchMediumFile, benchSlowFile, targetFile};
cleanupCommand = '';
for k = 1:length(files)
  cleanupCommand = [cleanupCommand, removeCommand(files{k}), ';'];
end
cleanup = onCleanup(@() evalc(cleanupCommand)); %#ok<NLS0004>

header = {'%============================================================================='; ...
  '% LICENCE_BLOCK_END'; ...
  '%============================================================================='};
filewrite(fastFile, [header; 'asserts.istrue(true);']);
filewrite(mediumFile, [header; '% <--WEIGHT 8-->'; 'asserts.istrue(true);']);
crlf = char([13 10]);
slowBytes = uint8(strjoin([header; 'asserts.istrue(true);'], crlf));
fid = fopen(slowFile, 'wb');
fwrite(fid, slowBytes, 'uint8');
fclose(fid);
filewrite(verySlowFile, [header; '% <--WEIGHT 2-->'; 'asserts.istrue(true);']);
filewrite(failedFile, [header; '% <--WEIGHT 16-->'; 'asserts.istrue(false);']);
filewrite(benchFastFile, [header; 'asserts.istrue(true);']);
filewrite(benchMediumFile, [header; 'asserts.istrue(true);']);
filewrite(benchSlowFile, [header; 'asserts.istrue(true);']);
filewrite(targetFile, [header; 'asserts.istrue(true);']);

caseTemplate = struct('id', '', 'file', '', 'module', 'synthetic', ...
  'kind', 'test', 'mode', 'cli', 'weight', 1, 'outcome', 'passed', ...
  'duration', 0, 'message', '', 'stdout', '', 'stderr', '', ...
  'exit_code', 0, 'attempts', 1, 'artifacts', {{}}, 'diagnostics', struct());
cases = repmat(caseTemplate, 1, 8);
caseFiles = {fastFile, mediumFile, slowFile, verySlowFile, failedFile, ...
  benchFastFile, benchMediumFile, benchSlowFile};
caseDurations = [1, 2, 8, 16, 100, 0.1, 0.2, 3];
for k = 1:length(cases)
  [~, name, ~] = fileparts(caseFiles{k}); %#ok<NLS0004>
  cases(k).id = name;
  cases(k).file = caseFiles{k};
  cases(k).duration = caseDurations(k);
end
cases(5).outcome = 'failed';
for k = 6:8
  cases(k).kind = 'bench';
  cases(k).outcome = 'bench';
end
results = struct('type', 'TestRunResult', 'status', false, 'cases', cases);

dryRun = nelson.unittest.tuneWeights(results); %#ok<NLS0005>
asserts.isequal(dryRun.type, 'WeightTuningResult');
asserts.isfalse(dryRun.applied);
asserts.isequal(dryRun.summary.eligible, 7);
asserts.isequal(dryRun.summary.added, 2);
asserts.isequal(dryRun.summary.updated, 1);
asserts.isequal(dryRun.summary.removed, 1);
asserts.isequal(dryRun.summary.unchanged, 3);
asserts.isequal(dryRun.summary.written, 0);
asserts.istrue(contains(fileread(mediumFile), '<--WEIGHT 8-->'));
asserts.isfalse(contains(fileread(slowFile), '<--WEIGHT'));
asserts.istrue(contains(fileread(failedFile), '<--WEIGHT 16-->'));

applied = nelson.unittest.tuneWeights(results, 'Apply', true);
asserts.istrue(applied.applied);
asserts.isequal(applied.summary.written, 4);
asserts.isequal(nelson.unittest.discover(mediumFile).cases.weight, 1);
asserts.isequal(nelson.unittest.discover(slowFile).cases.weight, 2);
asserts.isequal(nelson.unittest.discover(verySlowFile).cases.weight, 4);
asserts.isequal(nelson.unittest.discover(benchSlowFile).cases.weight, 16);
asserts.isfalse(contains(fileread(mediumFile), '<--WEIGHT'));
fid = fopen(slowFile, 'rb');
slowBytes = fread(fid, Inf, 'uint8')';
fclose(fid);
asserts.istrue(any(slowBytes(1:end - 1) == 13 & slowBytes(2:end) == 10));
asserts.istrue(contains(fileread(failedFile), '<--WEIGHT 16-->'));

stable = nelson.unittest.tuneWeights(results);
asserts.isequal(stable.summary.unchanged, 7);
limited = nelson.unittest.tuneWeights(results, 'MaxWeight', 8);
asserts.isequal(limited.cases(7).new_weight, 8);
asserts.isequal(limited.cases(7).action, 'update');

[targetReport, targetResults] = nelson.unittest.tuneWeights(targetFile, ...
  'Workers', 1, 'Kind', 'test', 'MaxWeight', 8);
asserts.isequal(targetReport.type, 'WeightTuningResult');
asserts.isequal(targetReport.summary.eligible, 1);
asserts.isequal(targetResults.type, 'TestRunResult');
asserts.isequal(targetResults.cases(1).file, targetFile);
asserts.isequal(targetResults.cases(1).kind, 'test');
asserts.isequal(targetResults.cases(1).mode, 'cli');
asserts.isequal(targetResults.cases(1).weight, 1);

asserts.checkerror('nelson.unittest.tuneWeights(results, ''Apply'', 1)', ...
  _('Apply must be a logical scalar.'));
asserts.checkerror('nelson.unittest.tuneWeights(results, ''MaxWeight'', 0)', ...
  _('MaxWeight must be a positive integer.'));
asserts.checkerror('nelson.unittest.tuneWeights(results, ''Workers'', 1)', ...
  _('Execution options are not accepted with a TestRunResult.'));
for k = 1:length(files)
  if isfile(files{k})
    rmfile(files{k});
  end
  asserts.isfalse(isfile(files{k}));
end
clear cleanup
%=============================================================================
