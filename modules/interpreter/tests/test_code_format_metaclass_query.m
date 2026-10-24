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
% <--WEIGHT 8-->
% The metaclass query "?ClassName" / "?pkg.ClassName" is kept as written by
% the formatter, and the comments after it are kept too.
workdir = tempname();
mkdir(workdir);
workdirCleanup = onCleanup(@() rmdir(workdir, 's')); %#ok<NASGU>
nl = char(10);
expected = ['function mq_case()', nl, ...
  '  m = ?A; % first', nl, ...
  '  p = ?pkg.Cls; % second', nl, ...
  '  n = ?A.Name;', nl, ...
  '  s = ''?B'';', nl, ...
  '  % ?C in a comment', nl, ...
  '  x = 1; % third', nl, ...
  'end', nl];
fixtureFile = [workdir, '/mq_case.m'];
%=============================================================================
% Already formatted: unchanged by smartindent.
filewrite(fixtureFile, expected);
smartindent(fixtureFile, 2, false, true);
formatted = strrep(fileread(fixtureFile, 'char', 'native'), char([13, 10]), nl);
asserts.isequal(formatted, expected);
%=============================================================================
% Unindented and unspaced: layout fixed, "?" tokens and comments kept.
unformatted = regexprep(expected, '(^|\n) +', '$1');
unformatted = strrep(unformatted, 'm = ?A;', 'm=?A;');
unformatted = strrep(unformatted, 'p = ?pkg.Cls;', 'p =?pkg.Cls;');
filewrite(fixtureFile, unformatted);
smartindent(fixtureFile, 2, false, true);
formatted = strrep(fileread(fixtureFile, 'char', 'native'), char([13, 10]), nl);
asserts.isequal(formatted, expected);
%=============================================================================
% classdef attributes: "?Name" lists keep no space after "?".
classSource = ['classdef (InferiorClasses = ?MqInferior) mq_class', nl, ...
  '  properties (GetAccess = {?MqFriend, ?pkg.MqPeer}, SetAccess = ?MqPeer)', nl, ...
  '    Value = 1; % kept', nl, ...
  '  end', nl, ...
  'end', nl];
classFile = [workdir, '/mq_class.m'];
filewrite(classFile, classSource);
smartindent(classFile, 2, false, true);
formatted = strrep(fileread(classFile, 'char', 'native'), char([13, 10]), nl);
asserts.isequal(formatted, classSource);
%=============================================================================
formatCommand = nelson_tool_command_for_test('nelson-format');
script = ['m = ?A;', nl, 'x = 1; % c', nl];
scriptFile = [workdir, '/mq_script.m'];
filewrite(scriptFile, script);
[status, output] = system([formatCommand, ' --check "', scriptFile, '"']);
asserts.isequal(status, 0);
asserts.isequal(output, '');
[status, output] = system([formatCommand, ' "', scriptFile, '"']);
asserts.isequal(status, 0);
formatted = strrep(fileread(scriptFile, 'char', 'native'), char([13, 10]), nl);
asserts.isequal(formatted, script);
[status, output] = system([formatCommand, ' --check "', classFile, '"']);
asserts.isequal(status, 0);
asserts.isequal(output, '');
filewrite(fixtureFile, unformatted);
[status, output] = system([formatCommand, ' "', fixtureFile, '"']);
asserts.isequal(status, 0);
formatted = strrep(fileread(fixtureFile, 'char', 'native'), char([13, 10]), nl);
asserts.isequal(formatted, expected);
%=============================================================================
