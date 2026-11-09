%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% A function called by another function, a script run inside a function and a
% recursive function are all listed by the profiler.
%=============================================================================
LF = char(10);
fixtureDirectory = tempname();
mkdir(fixtureDirectory);
fixtureDirectoryCleanup = onCleanup(@() rmdir(fixtureDirectory, 's')); %#ok<NLS0004>
filewrite(fullfile(fixtureDirectory, 'pnc_outer.m'), ['function r = pnc_outer(n)', LF, ...
  '  r = pnc_inner(n);', LF, 'end', LF]);
filewrite(fullfile(fixtureDirectory, 'pnc_inner.m'), ['function r = pnc_inner(n)', LF, ...
  '  if n > 0', LF, '    r = pnc_inner(n - 1) + 1;', LF, '  else', LF, '    r = 0;', LF, ...
  '  end', LF, 'end', LF]);
filewrite(fullfile(fixtureDirectory, 'pnc_script.m'), ['pncValue = pnc_inner(2);', LF]);
filewrite(fullfile(fixtureDirectory, 'pnc_runner.m'), ['function pnc_runner(f)', LF, ...
  '  run(f);', LF, 'end', LF]);
addpath(fixtureDirectory);
pathCleanup = onCleanup(@() rmpath(fixtureDirectory)); %#ok<NLS0004>
%=============================================================================
profile('clear');
profile('on');
value = pnc_outer(3);
pnc_runner(fullfile(fixtureDirectory, 'pnc_script.m'));
profile('off');
p = profile('info');
profile('clear');
asserts.isequal(value, 3);
names = {p.FunctionName};
asserts.istrue(any(strcmp(names, 'pnc_outer')));
asserts.istrue(any(strcmp(names, 'pnc_inner')));
asserts.istrue(any(strcmp(names, 'pnc_runner')));
asserts.istrue(any(strcmp(names, 'pnc_script')));
%=============================================================================
