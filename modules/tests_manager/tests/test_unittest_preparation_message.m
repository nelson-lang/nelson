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
currentPath = path();
cleanupPath = onCleanup(@() path(currentPath)); %#ok<NLS0004>
addpath([nelsonroot(), '/modules/tests_manager/functions/+nelson/+unittest/private']);

smallOutput = evalc('display_preparation_message(''prepare'', 49, 4);');
asserts.isequal(smallOutput, '');

prepareOutput = evalc('display_preparation_message(''prepare'', 50, 4);');
asserts.istrue(contains(prepareOutput, 'Preparing 50 tests for 4 workers.'));

runOutput = evalc('display_preparation_message(''run'', 75, 8);');
asserts.istrue(contains(runOutput, ...
  'Running 75 jobs; results are displayed in test order.'));

previousProgress = getenv('NELSON_UNITTEST_PROGRESS_ANIMATION');
cleanupProgress = onCleanup(...
  @() setenv('NELSON_UNITTEST_PROGRESS_ANIMATION', previousProgress)); %#ok<NLS0004>
setenv('NELSON_UNITTEST_PROGRESS_ANIMATION', '1');

prepareOutput = evalc('display_preparation_message(''prepare'', 50, 4);');
asserts.isequal(prepareOutput, '');

progressOutput = evalc([...
  'p = progress_indicator_start(''Preparing tests'', 50);', ...
  'p = progress_indicator_update(p, 25);', ...
  'progress_indicator_finish(p, 50);']);
asserts.istrue(contains(progressOutput, 'Preparing tests: 0 %'));
asserts.istrue(contains(progressOutput, '50 %'));
asserts.istrue(contains(progressOutput, '100 %'));
%=============================================================================
