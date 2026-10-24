%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 16-->
% Inside [] and {}, "a -b" (space before the sign, none after) is two elements
% while "a - b" and "a-b" are one subtraction: the formatter must never turn the
% former into the latter.
folder = tempname(tempdir());
mkdir(folder);
cleanup = onCleanup(@() rmdir(folder, 's'));
setup = 'a = [5 7]; b = [2 3]; x = 3;';
% {input, expected formatted text}
cases = {};
cases{end + 1} = {'r = [-0.11 -0.07 -0.03 0 0.03 0.07 0.11];', ''};
cases{end + 1} = {'r = [a -b];', ''};
cases{end + 1} = {'r = [a +b];', ''};
cases{end + 1} = {'r = {1 -2};', ''};
cases{end + 1} = {'r = [x -abs(-1)];', ''};
cases{end + 1} = {'r = [x -x''];', ''};
cases{end + 1} = {'r = [a'' -b''];', ''};
cases{end + 1} = {'r = [a.'' -b.''];', ''};
cases{end + 1} = {'r = [a'' b''];', ''};
cases{end + 1} = {'r = [1 -2; 3 -4];', ''};
cases{end + 1} = {'r = [x -(1)];', ''};
cases{end + 1} = {'r = [x (1)];', ''};
cases{end + 1} = {'r = {x (1)};', ''};
cases{end + 1} = {'r = [x'' (5)];', ''};
cases{end + 1} = {'r = [x(1) (2)];', ''};
cases{end + 1} = {'r = [a -~b];', ''};
cases{end + 1} = {'r = [[a -b] -[1 2]];', ''};
cases{end + 1} = {'r = {{1 -2} -3};', ''};
cases{end + 1} = {'r = [a -b]'';', ''};
cases{end + 1} = {'r = [a(1) -b(1)];', ''};
cases{end + 1} = {'r = [a - b];', ''};
cases{end + 1} = {'r = [a-b];', 'r = [a - b];'};
cases{end + 1} = {'r = [a'' - b''];', ''};
cases{end + 1} = {'r = a -b;', 'r = a - b;'};
cases{end + 1} = {'r = [1 -2 ...', '  -3];', ''};
cases{end + 1} = {'r = [1 2 ...', '  3 -4];', ''};
cases{end + 1} = {'r = [1 2', '3 -4];', ['r = [1 2', char(10), '  3 -4];']};
cases{end + 1} = {'r = [1 2', '  3 -4];', ''};
cases{end + 1} = {'r = {1, ...', '  x -b};', ''};
cases{end + 1} = {'r = x', ';', 'r = max(a, b);', ''};
cases{end + 1} = {'r = [1; 2;; 3];;', ''};
cases{end + 1} = {'r = [x ...', '  - (1)];', ''};
cases{end + 1} = {'r = [x ...', '  -b];', ''};
cases{end + 1} = {'r = [x ... a comment', '  (1)];', ''};
cases{end + 1} = {'r = x + ... a comment', '  2;', ''};
for k = 1:numel(cases)
  input = cases{k}(1:end - 1);
  expected = cases{k}{end};
  if isempty(expected)
    expected = strjoin(input, char(10));
  end
  source = strjoin([{setup}, input], char(10));
  filename = fullfile(folder, sprintf('signed_%d.m', k));
  filewrite(filename, source);
  expectedValue = evaluateSignedFixture(filename);
  smartindent(filename);
  formatted = strrep(fileread(filename), char([13, 10]), char(10));
  asserts.isequal(formatted, [setup, char(10), expected]);
  asserts.isequal(evaluateSignedFixture(filename), expectedValue);
  % Idempotent: a second pass leaves the text unchanged.
  smartindent(filename);
  asserts.isequal(strrep(fileread(filename), char([13, 10]), char(10)), formatted);
end
%=============================================================================
% Command syntax keeps its literal arguments.
commandFile = fullfile(folder, 'command_syntax.m');
commandSource = ['disp -abc', char(10), 'disp +abc'];
filewrite(commandFile, commandSource);
smartindent(commandFile);
asserts.isequal(strrep(fileread(commandFile), char([13, 10]), char(10)), commandSource);
%=============================================================================
% classdef methods are formatted by the text fallback: same rules.
classFile = fullfile(folder, 'SignedElementsProbe.m');
classSource = strjoin({'classdef SignedElementsProbe', ...
  '  properties', ...
  '    v = [1 -2];', ...
  '    tol = 1e-6;', ...
  '    w = 1 + ... a comment', ...
  '      2;', ...
  '  end', ...
  '  methods (Static)', ...
  '    function r = f(a, b, x)', ...
  '      r = {[a'' -b''], [a.'' -b.''], [x -(1)], [a -b], [a'' b'']};', ...
  '    end', ...
  '    function [r, t] = g(s, x)', ...
  '      t = 2.5e+3;', ...
  '', ...
  '      if x > 0 ...', ...
  '        && x < 9', ...
  '        t = [x ...', ...
  '          - (1)];', ...
  '      end', ...
  '      switch s', ...
  '        case ''codes''', ...
  '          r = [x ''abc'' (1)];', ...
  '        otherwise', ...
  '          r = {x'', x''''};', ...
  '      end', ...
  '    end', ...
  '  end', ...
  'end'}, char(10));
filewrite(classFile, classSource);
smartindent(classFile);
asserts.isequal(strrep(fileread(classFile), char([13, 10]), char(10)), classSource);
%=============================================================================
% A UTF-8 byte order mark or CRLF line endings must not shift the rules; the
% command line tool formats the raw bytes.
formatCommand = nelson_tool_command_for_test('nelson-format');
% Outside the repository: its temp/ folder is on the formatter ignore list.
cliFolder = tempname();
mkdir(cliFolder);
cliCleanup = onCleanup(@() rmdir(cliFolder, 's'));
body = ['x = 1;', char([13, 10]), 'r = [-0.11 -0.07 -0.03 0 0.03 0.07 0.11];', char([13, 10]), ...
  'r = [x -1];', char([13, 10])];
bomBytes = [uint8([239, 187, 191]), uint8(body)];
variants = {uint8(body), bomBytes, uint8(strrep(body, char([13, 10]), char(10)))};
for k = 1:numel(variants)
  rawFile = fullfile(cliFolder, sprintf('raw_%d.m', k));
  fw = fopen(rawFile, 'wb');
  fwrite(fw, variants{k}, 'uint8');
  fclose(fw);
  [status, output] = system([formatCommand, ' "', rawFile, '"']);
  asserts.isequal(status, 0);
  fr = fopen(rawFile, 'rb');
  bytes = fread(fr, Inf, 'uint8=>uint8')';
  fclose(fr);
  asserts.isequal(bytes, variants{k});
end
%=============================================================================
% A text literal spanning lines is kept byte for byte, only its first line is
% re-indented.
nl = char(10);
textFile = fullfile(folder, 'multiline_text.m');
textBody = ['A =', nl, nl, '   1.8000e+99 - 2.2204e-16i  ', nl, '  '';'];
filewrite(textFile, ['if true', nl, 'r = ''', nl, textBody, nl, 'end']);
expectedValue = evaluateSignedFixture(textFile);
smartindent(textFile);
asserts.isequal(strrep(fileread(textFile), char([13, 10]), nl), ['if true', nl, '  r = ''', nl, textBody, nl, 'end']);
asserts.isequal(evaluateSignedFixture(textFile), expectedValue);
%=============================================================================
% A malformed number no longer hangs the command line tool, nor gets rewritten.
badFile = fullfile(cliFolder, 'lexical_error.m');
filewrite(badFile, 'x = 1.8e + 99;');
[status, output] = system([formatCommand, ' "', badFile, '"']);
asserts.isequal(fileread(badFile), 'x = 1.8e + 99;');
%=============================================================================
function r = evaluateSignedFixture(filename)
  run(filename);
end
%=============================================================================
