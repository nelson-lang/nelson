%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
testDirectory = [tempdir(), 'test_unittest_inline_warnings_', createGUID()];
mkdir(testDirectory);
filename = [testDirectory, '/test_warning_fixture.m'];
%#ok<NLS0004>
cleanupFiles = onCleanup(@() evalc([...
  'if isdir(''', testDirectory, '''), rmdir(''', testDirectory, ''', ''s''), end']));
savedWarningState = warning();
[savedWarningMessage, savedWarningIdentifier] = lastwarn();
%#ok<NLS0004>
cleanupWarningState = onCleanup(@() warning(savedWarningState));
%#ok<NLS0004>
cleanupWarning = onCleanup(@() lastwarn(savedWarningMessage, savedWarningIdentifier));
lastwarn('Caller warning', 'Nelson:test:callerWarning');

for failFixture = [false, true]
  content = {
    'asserts.isequal(lastwarn(), '''');'
    'asserts.isequal(getLastReport(), '''');'
    'warning(''off'', ''Nelson:test:fixtureWarning'');'
    'warning(''Nelson:test:fixtureWarning'', ''Fixture warning'');'
    };
  if failFixture
    content{end + 1} = 'error(''Nelson:test:warningFixture'', ''Intentional fixture failure.'');';
  end
  filewrite(filename, content);
  lasterr('Caller error', 'Nelson:test:callerError');
  result = worker_pool_safe_runfile(filename, true);
  asserts.isequal(result.r ~= 0, failFixture);
  asserts.isequal(warning(), savedWarningState);
  [message, identifier] = lastwarn();
  asserts.isequal(message, 'Caller warning');
  asserts.isequal(identifier, 'Nelson:test:callerWarning');
  asserts.isequal(getLastReport(), '');
end
%=============================================================================
