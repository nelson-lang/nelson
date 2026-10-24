%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 3-->
% setenv('NELSON_UNITTEST_TRACE', '1') from a script must enable the native
% process supervisor traces, not only the .m side traces.
binpath = modulepath('nelson', 'bin');
nelson_exe = [binpath, '/nelson-cli'];
if ispc()
  nelson_exe = [nelson_exe, '.exe'];
end
skip_testsuite(~isfile(nelson_exe), ['Executable not found: ', nelson_exe]);

removeCommand = @(filename)['if isfile(''', filename, '''), ', ...
  'rmfile(''', filename, '''), end'];
literal = @(value)['''', strrep(value, '''', ''''''), ''''];
base = [tempdir(), 'test_unittest_trace_setenv_', createGUID()];
testFile = [base, '_test_probe.m'];
driverFile = [base, '_driver.m'];
stderrFile = [base, '_stderr.txt'];
cleanupTestFile = onCleanup(@() evalc(removeCommand(testFile))); %#ok<NLS0004>
cleanupDriverFile = onCleanup(@() evalc(removeCommand(driverFile))); %#ok<NLS0004>
cleanupStderrFile = onCleanup(@() evalc(removeCommand(stderrFile))); %#ok<NLS0004>

filewrite(testFile, {'asserts.istrue(true);'});
filewrite(driverFile, {
  'setenv(''NELSON_UNITTEST_TRACE'', ''1'');'
  ['suite = nelson.unittest.discover(', literal(testFile), ', ''Kind'', ''test'');']
  'testPlan = nelson.unittest.plan(suite, ''Workers'', 1);'
  'results = nelson.unittest.run(testPlan);'
  'exit(double(~results.status), ''force'');'
  });

% The child must start without the trace variables inherited from this
% process, so only the setenv call inside the driver can enable them, and
% without the worker marker so its run behaves as a top-level run.
% Traces go to stderr, which system() drops on success: capture it in a file.
clearedVariables = {'NELSON_UNITTEST_TRACE', 'NELSON_UNITTEST_TRACE_PROGRESSIVE', ...
  'NELSON_UNITTEST_RUNFILE_WORKER'};
nelsonCommand = ['"', nelson_exe, '" --quiet --no-ipc --no-user-modules -f "', ...
  driverFile, '" 2> "', stderrFile, '"'];
if ispc()
  clearPrefix = sprintf('set "%s=" && ', clearedVariables{:});
else
  clearPrefix = ['env', sprintf(' -u %s', clearedVariables{:}), ' '];
end
cmd = [clearPrefix, nelsonCommand];
[status, output] = system(cmd, 120);

asserts.isequal(status, 0);
% The summary prints the passed count after a status glyph and padding
% ("Passed:  <emoji> 1"), so match the count tolerantly rather than a fixed
% "Passed: 1" literal.
asserts.istrue(~isempty(regexp(output, 'Passed:\D*1(\D|$)', 'once')));
asserts.istrue(isfile(stderrFile));
traces = fileread(stderrFile);
% .m side traces (Nelson getenv)
asserts.istrue(contains(traces, '[tests_manager progressive] start cases=1'));
% native supervisor traces (C++ side)
asserts.istrue(contains(traces, '[tests_manager +'));
asserts.istrue(contains(traces, '] start jobs=1 workers=1'));
asserts.istrue(contains(traces, '] done'));
%=============================================================================
