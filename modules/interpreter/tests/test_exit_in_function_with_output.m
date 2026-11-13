%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 32-->
skip_testsuite((strcmp(getenv('NELSON_PORTABLE_TEST_RUN'), '1') || iswasm()), ...
  'Native nelson-cli exit-code propagation is not available in the portable runtime.');
%=============================================================================
% exit inside a function with an unassigned output (called through evalc and
% run errcatch) must end the process with its code, not raise an output error.
binpath = modulepath('nelson', 'bin');
if ispc()
  nelson_exe = ['"', binpath, '/nelson-cli', '"'];
else
  nelson_exe = [binpath, '/nelson-cli'];
end
removeCommand = @(filename)['if isfile(''', filename, '''), ', ...
  'rmfile(''', filename, '''), end'];
base = [[tempdir(), ''], 'exitfn_', strrep(createGUID(), '-', '')];
[~, functionName] = fileparts(base);
functionName = [functionName, '_f'];
functionFile = [base, '_f.m'];
innerScript = [base, '_inner.m'];
mainScript = [base, '_main.m'];
cleanupFiles = onCleanup(@() evalc([removeCommand(functionFile), newline, ...
  removeCommand(innerScript), newline, removeCommand(mainScript)])); %#ok<NLS0004>
filewrite(functionFile, {['function y = ', functionName, '()']; ...
  '  exit(5, ''force'');'; '  y = 1;'; 'end'});
filewrite(innerScript, {['r = ', functionName, '();']});
filewrite(mainScript, {['addpath(''', fileparts(base), ''');']; ...
  ['out = evalc(''st = run(''''', innerScript, ''''', ''''errcatch'''');'');']; ...
  'exit(0, ''force'');'});
%=============================================================================
cmd = [nelson_exe, ' --quiet --no-user-startup --file "', mainScript, '"'];
[R, msg] = system(cmd);
asserts.isequal(R, 5);
asserts.isfalse(contains(msg, 'is not assigned'));
%=============================================================================
