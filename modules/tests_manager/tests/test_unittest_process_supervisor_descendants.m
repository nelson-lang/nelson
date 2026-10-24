%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 8-->
% <--SEQUENTIAL TEST REQUIRED-->
skip_if_no_native_unittest_runner();
%=============================================================================
removeCommand = @(filename)['if isfile(''', filename, '''), ', ...
  'rmfile(''', filename, '''), end'];
previousForceNativeRunner = getenv('NELSON_UNITTEST_FORCE_NATIVE_RUNNER');
cleanupForceNativeRunner = onCleanup(...
  @() setenv('NELSON_UNITTEST_FORCE_NATIVE_RUNNER', previousForceNativeRunner)); %#ok<NLS0004>
setenv('NELSON_UNITTEST_FORCE_NATIVE_RUNNER', 'TRUE');
base = [[tempdir(), ''], 'test_unittest_process_supervisor_descendants_', createGUID()];
testFile = [base, '.m'];
markerFile = [base, '.marker'];
normalTestFile = [base, '_normal.m'];
normalMarkerFile = [base, '_normal.marker'];
if ispc()
  childScript = [base, '.ps1'];
  normalChildScript = [base, '_normal.ps1'];
  launcherScript = '';
  normalLauncherScript = '';
else
  childScript = [base, '.sh'];
  normalChildScript = [base, '_normal.sh'];
  launcherScript = [base, '_launcher.sh'];
  normalLauncherScript = [base, '_normal_launcher.sh'];
end
cleanupTest = onCleanup(@() evalc(removeCommand(testFile))); %#ok<NLS0004>
cleanupMarker = onCleanup(@() evalc(removeCommand(markerFile))); %#ok<NLS0004>
cleanupChild = onCleanup(@() evalc(removeCommand(childScript))); %#ok<NLS0004>
cleanupNormalTest = onCleanup(@() evalc(removeCommand(normalTestFile))); %#ok<NLS0004>
cleanupNormalMarker = onCleanup(@() evalc(removeCommand(normalMarkerFile))); %#ok<NLS0004>
cleanupNormalChild = onCleanup(@() evalc(removeCommand(normalChildScript))); %#ok<NLS0004>
if ~ispc()
  cleanupLauncher = onCleanup(@() evalc(removeCommand(launcherScript))); %#ok<NLS0004>
  cleanupNormalLauncher = onCleanup(@() evalc(removeCommand(normalLauncherScript))); %#ok<NLS0004>
end

if ispc()
  filewrite(childScript, {...
    'Start-Sleep -Seconds 30'
    ['Set-Content -LiteralPath ''', markerFile, ''' -Value ''orphan''']});
  filewrite(normalChildScript, {...
    'Start-Sleep -Seconds 30'
    ['Set-Content -LiteralPath ''', normalMarkerFile, ''' -Value ''orphan''']});
  launchCommand = ['unix(''cmd /c start "" /b powershell -NoProfile ', ...
    '-ExecutionPolicy Bypass -File "', childScript, '"'');'];
  normalLaunchCommand = ['unix(''cmd /c start "" /b powershell -NoProfile ', ...
    '-ExecutionPolicy Bypass -File "', normalChildScript, '"'');'];
else
  filewrite(childScript, {...
    '#!/bin/sh'
    'sleep 30'
    ['printf orphan > "', markerFile, '"']});
  filewrite(normalChildScript, {...
    '#!/bin/sh'
    'sleep 30'
    ['printf orphan > "', normalMarkerFile, '"']});
  filewrite(launcherScript, {...
    '#!/bin/sh'
    ['sh "', childScript, '" &']
    'sleep 30'});
  filewrite(normalLauncherScript, {...
    '#!/bin/sh'
    ['sh "', normalChildScript, '" &']});
  launchCommand = ['unix(''sh "', launcherScript, '"'');'];
  normalLaunchCommand = ['unix(''sh "', normalLauncherScript, '"'');'];
end
if ispc()
  filewrite(testFile, {
    launchCommand
    'pause(30);'
    });
  filewrite(normalTestFile, {
    normalLaunchCommand
    'asserts.istrue(true);'
    });

  suite = nelson.unittest.discover(testFile, 'Kind', 'test');
  testPlan = nelson.unittest.plan(suite, 'Workers', 1);
  results = nelson.unittest.run(testPlan, 'Timeout', 3);
  asserts.isfalse(results.status);
  asserts.isequal(results.cases(1).outcome, 'timeout');
  pause(4);
  asserts.isfalse(isfile(markerFile));

  suite = nelson.unittest.discover(normalTestFile, 'Kind', 'test');
  testPlan = nelson.unittest.plan(suite, 'Workers', 1);
  results = nelson.unittest.run(testPlan, 'Timeout', 10);
  asserts.istrue(results.status);
  asserts.isequal(results.cases(1).outcome, 'passed');
  pause(4);
  asserts.isfalse(isfile(normalMarkerFile));
else
  jobs = run_process_jobs({'/bin/sh'}, {['"', launcherScript, '"']}, ...
    3, {'timeout'}, 1, {'descendants'}, {'timeout-descendants'});
  results = run_processes(jobs, 1);
  asserts.isequal(results.reason, 'timeout');
  asserts.istrue(results.timed_out);
  pause(4);
  asserts.isfalse(isfile(markerFile));

  jobs = run_process_jobs({'/bin/sh'}, {['"', normalLauncherScript, '"']}, ...
    10, {'normal'}, 1, {'descendants'}, {'normal-descendants'});
  results = run_processes(jobs, 1);
  asserts.isequal(results.reason, 'ok');
  asserts.isfalse(results.timed_out);
  pause(4);
  asserts.isfalse(isfile(normalMarkerFile));
end
%=============================================================================
