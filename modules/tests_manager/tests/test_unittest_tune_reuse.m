%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 16-->
skip_if_no_native_unittest_runner();
%=============================================================================
removeCommand = @(filename)['if isfile(''', filename, '''), ', ...
  'rmfile(''', filename, '''), end'];
base = [[tempdir(), ''], 'test_unittest_tune_reuse_', createGUID()];
safeTaggedFile = [base, '_safe_tagged.m'];
unsafeTaggedFile = [base, '_unsafe_tagged.m'];
safeUntaggedFile = [base, '_safe_untagged.m'];
guiTaggedFile = [base, '_gui_tagged.m'];
deferredFile = [base, '_deferred.m'];
files = {safeTaggedFile, unsafeTaggedFile, safeUntaggedFile, ...
  guiTaggedFile, deferredFile};
cleanupCommand = '';
for k = 1:length(files)
  cleanupCommand = [cleanupCommand, removeCommand(files{k}), ';'];
end
cleanup = onCleanup(@() evalc(cleanupCommand)); %#ok<NLS0004>

header = {'%============================================================================='; ...
  '% LICENCE_BLOCK_END'; ...
  '%============================================================================='};
filewrite(safeTaggedFile, [header; ...
  '% <--REUSE PROCESS-->'; ...
  'asserts.istrue(true);']);
marker = ['NELSON_UNITTEST_TUNE_REUSE_', replace(createGUID(), '-', '_')];
filewrite(unsafeTaggedFile, [header; ...
  '% <--REUSE PROCESS-->'; ...
  ['global ', marker, ';']; ...
  ['if isempty(', marker, ')']; ...
  ['  ', marker, ' = true;']; ...
  'else'; ...
  '  asserts.istrue(false);'; ...
  'end']);
crlf = char([13 10]);
safeBytes = unicode2native(strjoin([header; 'asserts.istrue(true);'], crlf), 'UTF-8');
safeBytes = [uint8([239 187 191]), safeBytes];
fid = fopen(safeUntaggedFile, 'wb');
fwrite(fid, safeBytes, 'uint8');
fclose(fid);
filewrite(guiTaggedFile, [header; ...
  '% <--GUI MODE-->'; ...
  '% <--REUSE PROCESS-->'; ...
  'asserts.istrue(true);']);
filewrite(deferredFile, [header; 'asserts.istrue(true);']);

deferred = nelson.unittest.tuneReuse(deferredFile);
asserts.isequal(deferred.type, 'ReuseTuningResult');
asserts.isfalse(deferred.applied);
asserts.isequal(deferred.summary.selected, 1);
asserts.isequal(deferred.summary.eligible, 1);
asserts.isequal(deferred.summary.calibrated, 0);
asserts.isequal(deferred.summary.deferred, 1);
asserts.isfalse(contains(fileread(deferredFile), '<--REUSE PROCESS-->'));

targets = files(1:4); %#ok<NLS0004>
previousForceNative = getenv('NELSON_UNITTEST_FORCE_NATIVE_RUNNER');
output = evalc(['[report, calibration] = nelson.unittest.tuneReuse(targets, ', ...
  '''Trials'', 1, ''MinGain'', -1, ''AllowAdd'', true, ''Apply'', true);']);
asserts.isequal(getenv('NELSON_UNITTEST_FORCE_NATIVE_RUNNER'), previousForceNative);
asserts.istrue(contains(output, 'Reuse calibration'));
asserts.isequal(report.type, 'ReuseTuningResult');
asserts.istrue(report.applied);
asserts.istrue(report.allow_add);
asserts.istrue(report.trials == 1, 'trials');
asserts.istrue(report.summary.selected == 4, 'summary.selected');
asserts.istrue(report.summary.eligible == 3, 'summary.eligible');
asserts.istrue(report.summary.calibrated == 3, 'summary.calibrated');
asserts.istrue(report.summary.excluded == 1, 'summary.excluded');
asserts.istrue(report.summary.added == 1, jsonencode(report.summary));
asserts.istrue(report.summary.removed == 1, 'summary.removed');
asserts.istrue(report.summary.unchanged == 1, 'summary.unchanged');
asserts.istrue(report.summary.written == 2, 'summary.written');
asserts.istrue(report.summary.reuse_failed == 1, 'summary.reuse_failed');
asserts.istrue(report.summary.certified == 1, 'summary.certified');
asserts.isequal(calibration.type, 'ReuseCalibration');
asserts.istrue(length(calibration.baseline.cases) == 6, 'baseline.cases');
asserts.istrue(length(calibration.reuse_trials{1}.cases) == 6, 'reuse.cases');

reportFiles = cell(1, length(report.cases));
for k = 1:length(report.cases)
  reportFiles{k} = report.cases(k).file;
end
safeTagged = report.cases(find(strcmp(reportFiles, safeTaggedFile), 1));
unsafeTagged = report.cases(find(strcmp(reportFiles, unsafeTaggedFile), 1));
safeUntagged = report.cases(find(strcmp(reportFiles, safeUntaggedFile), 1));
asserts.isequal(safeTagged.action, 'unchanged');
asserts.isequal(safeTagged.reason, 'certified');
asserts.istrue(safeTagged.baseline_ok);
asserts.istrue(safeTagged.reuse_ok);
asserts.isequal(unsafeTagged.action, 'remove');
asserts.isequal(unsafeTagged.reason, 'reuse_failed');
asserts.istrue(unsafeTagged.baseline_ok);
asserts.isfalse(unsafeTagged.reuse_ok);
asserts.isequal(safeUntagged.action, 'add');
asserts.istrue(any(strcmp({'safe_and_beneficial', 'safety_only'}, safeUntagged.reason)));

safeTaggedCase = nelson.unittest.discover(safeTaggedFile).cases;
unsafeTaggedCase = nelson.unittest.discover(unsafeTaggedFile).cases;
safeUntaggedCase = nelson.unittest.discover(safeUntaggedFile).cases;
guiTaggedCase = nelson.unittest.discover(guiTaggedFile).cases;
asserts.istrue(any(strcmp(safeTaggedCase.tags, 'reuse_process')));
asserts.isfalse(any(strcmp(unsafeTaggedCase.tags, 'reuse_process')));
asserts.istrue(any(strcmp(safeUntaggedCase.tags, 'reuse_process')));
asserts.istrue(any(strcmp(guiTaggedCase.tags, 'reuse_process')));
fid = fopen(safeUntaggedFile, 'rb');
safeBytes = fread(fid, Inf, 'uint8')';
fclose(fid);
asserts.isequal(safeBytes(1:3), uint8([239 187 191]));
asserts.istrue(any(safeBytes(1:end - 1) == 13 & safeBytes(2:end) == 10));

asserts.checkerror('nelson.unittest.tuneReuse(deferredFile, ''Apply'', 1)', ...
  _('Apply must be a logical scalar.'));
asserts.checkerror('nelson.unittest.tuneReuse(deferredFile, ''AllowAdd'', 1)', ...
  _('AllowAdd must be a logical scalar.'));
asserts.checkerror('nelson.unittest.tuneReuse(deferredFile, ''Trials'', 0)', ...
  _('Trials must be a positive integer.'));
asserts.checkerror('nelson.unittest.tuneReuse(deferredFile, ''MinGain'', 2)', ...
  _('MinGain must be between -1 and 1.'));
asserts.checkerror('nelson.unittest.tuneReuse(deferredFile, ''Workers'', 1)', ...
  _('Unsupported option.'));
for k = 1:length(files)
  if isfile(files{k})
    rmfile(files{k});
  end
  asserts.isfalse(isfile(files{k}));
end
clear cleanup
%=============================================================================
