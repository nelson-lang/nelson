%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--SEQUENTIAL TEST REQUIRED-->
skip_if_no_native_unittest_runner();
%=============================================================================
currentPath = path();
cleanupPath = onCleanup(@() path(currentPath)); %#ok<NLS0004>
addpath([nelsonroot(), '/modules/tests_manager/functions/+nelson/+unittest/private']);
previousForce = getenv('NELSON_UNITTEST_FORCE_NATIVE_RUNNER');
cleanupEnv = onCleanup(...
  @() setenv('NELSON_UNITTEST_FORCE_NATIVE_RUNNER', previousForce)); %#ok<NLS0004>
setenv('NELSON_UNITTEST_FORCE_NATIVE_RUNNER', 'TRUE');
base = [[tempdir(), ''], 'test_unittest_progressive_display_', createGUID()];
testFiles = {[base, '_one.m']; [base, '_two.m']; [base, '_three.m']};
cleanupCommand = '';
for k = 1:length(testFiles)
  cleanupCommand = [cleanupCommand, ...
    'if isfile(''', testFiles{k}, '''), rmfile(''', testFiles{k}, '''), end;', newline];
end
cleanupFiles = onCleanup(@() evalc(cleanupCommand)); %#ok<NLS0004>

filewrite(testFiles{1}, {'pause(0.4);'; 'asserts.istrue(true);'});
filewrite(testFiles{2}, {'asserts.istrue(true);'});
filewrite(testFiles{3}, {'asserts.istrue(true);'});
plan = nelson.unittest.plan(...
  nelson.unittest.discover(testFiles, 'Kind', 'test'), 'Workers', 3);
txt = evalc('results = nelson.unittest.run(plan);');
asserts.istrue(results.status);
positions = [firstMatch(txt, '_one'), firstMatch(txt, '_two'), firstMatch(txt, '_three')];
asserts.istrue(all(positions > 0));
asserts.istrue(issorted(positions));
asserts.istrue(contains(txt, 'Pass[') || contains(txt, [getUnicodeStatusCharacter('Pass'), '[']));
asserts.istrue(contains(txt, 's]'));
%=============================================================================
function position = firstMatch(txt, pattern)
  positions = strfind(txt, pattern);
  if isempty(positions)
    position = 0;
  else
    position = positions(1);
  end
end
%=============================================================================
