%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ENGLISH IMPOSED-->
%=============================================================================
% A local function is shown and addressed as file>local (dbstack display, error
% report, which, dbstop and dbstatus); dbstack and error stacks name it alone.
%=============================================================================
LF = char(10);
fixtureDirectory = tempname();
mkdir(fixtureDirectory);
fixtureDirectoryCleanup = onCleanup(@() rmdir(fixtureDirectory, 's')); %#ok<NLS0004>
filewrite(fullfile(fixtureDirectory, 'qnf.m'), ['function out = qnf(mode)', LF, ...
  '  out = qnf_local(mode);', LF, ...
  'end', LF, ...
  'function out = qnf_local(mode)', LF, ...
  '  if strcmp(mode, ''display'')', LF, ...
  '    out = evalc(''dbstack'');', LF, ...
  '  elseif strcmp(mode, ''complete'')', LF, ...
  '    out = evalc(''dbstack(''''-completenames'''')'');', LF, ...
  '  elseif strcmp(mode, ''names'')', LF, ...
  '    st = dbstack();', LF, ...
  '    out = {st.name};', LF, ...
  '  else', LF, ...
  '    error(''qnf:failure'', ''local failure'');', LF, ...
  '  end', LF, ...
  'end', LF]);
filewrite(fullfile(fixtureDirectory, 'qns.m'), ['qnsDisplay = qns_second();', LF, ...
  'function out = qns_first()', LF, ...
  '  out = 0;', LF, ...
  'end', LF, ...
  'function out = qns_second()', LF, ...
  '  out = evalc(''dbstack'');', LF, ...
  'end', LF]);
addpath(fixtureDirectory);
pathCleanup = onCleanup(@() rmpath(fixtureDirectory)); %#ok<NLS0004>
dbclear all
%=============================================================================
% dbstack display: file>local for a local function, the name for the main one
txt = qnf('display');
asserts.istrue(contains(txt, 'In qnf>qnf_local (line 6)'));
asserts.istrue(contains(txt, 'In qnf (line 2)'));
asserts.isfalse(contains(txt, ' function '));
txt = qnf('complete');
asserts.istrue(contains(txt, 'qnf.m>qnf_local (line 8)'));
asserts.istrue(contains(txt, 'qnf.m>qnf (line 2)'));
run(fullfile(fixtureDirectory, 'qns.m'));
asserts.istrue(contains(qnsDisplay, 'In qns>qns_second (line 6)'));
asserts.isfalse(contains(qnsDisplay, 'qns_first'));
%=============================================================================
% dbstack and error stacks keep the local name alone
names = qnf('names');
asserts.isequal(names{1}, 'qnf_local');
asserts.isequal(names{2}, 'qnf');
e = [];
try
  qnf('error');
catch e
end
stackNames = {e.stack.name};
asserts.istrue(any(strcmp(stackNames, 'qnf_local')));
asserts.isfalse(any(contains(stackNames, '>')));
asserts.istrue(contains(getReport(e), 'Error in qnf>qnf_local (line 13)'));
%=============================================================================
% the profiler lists a local function as file>local too
profile('clear');
profile('on');
names = qnf('names');
profile('off');
p = profile('info');
profile('clear');
profiledNames = {p.FunctionName};
asserts.istrue(any(strcmp(profiledNames, 'qnf>qnf_local')));
asserts.isfalse(any(strcmp(profiledNames, 'qnf_local')));
%=============================================================================
% which resolves file>local to the file, and to nothing when the local is missing
expected = which('qnf');
asserts.isequal(which('qnf>qnf_local'), expected);
asserts.isequal(which('qnf>missing_local'), '');
asserts.isequal(which('missing_file>qnf_local'), '');
txt = evalc('which qnf>qnf_local');
asserts.istrue(contains(txt, [expected, ' (qnf_local)  % Local function of qnf']));
%=============================================================================
% dbstop and dbstatus take and show the same form
dbstop in qnf>qnf_local
b = dbstatus('qnf');
asserts.isequal(b.name, 'qnf>qnf_local');
txt = evalc('dbstatus qnf');
asserts.istrue(contains(txt, 'Breakpoint for qnf>qnf_local is on line 5.'));
dbclear all
%=============================================================================
