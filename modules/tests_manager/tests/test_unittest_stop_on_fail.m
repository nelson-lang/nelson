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
base = [[tempdir(), ''], 'test_unittest_stop_on_fail_', createGUID()];
failFile = [base, '_test_fail_first.m'];
passFile = [base, '_test_pass_second.m'];
markerFile = [base, '_marker.txt'];
cleanupFail = onCleanup(@() evalc(removeCommand(failFile))); %#ok<NLS0004>
cleanupPass = onCleanup(@() evalc(removeCommand(passFile))); %#ok<NLS0004>
cleanupMarker = onCleanup(@() evalc(removeCommand(markerFile))); %#ok<NLS0004>

filewrite(failFile, {'asserts.istrue(false);'});
filewrite(passFile, {['filewrite(''', markerFile, ''', ''ran'');']; 'asserts.istrue(true);'});

results = nelson.unittest.run({failFile; passFile}, 'Workers', 1, 'StopOnFail', true);
asserts.isfalse(results.status);
asserts.isequal(length(results.cases), 1);
asserts.isequal(results.cases(1).outcome, 'failed');
asserts.isfalse(isfile(markerFile));
%=============================================================================
