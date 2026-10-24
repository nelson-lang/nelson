%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
folder = tempname(fullfile(nelsonroot(), 'temp'));
mkdir(folder);
originalPath = path();
cleanup = onCleanup(@() cleanupFixtureFolder(folder, originalPath));
cases = {};
values = [];
cases{end + 1} = {'if true, result = 1; end', 'tail = 2;', 'result = result + tail;'};
values(end + 1) = 3;
cases{end + 1} = {'result = 0; if true', '  result = 1;', 'end', 'result = result + 1;'};
values(end + 1) = 2;
cases{end + 1} = {'result = 0;', 'for k = 1:3', ...
  '  if k == 2, continue; end', '  result = result + k;', 'end', 'result = result + 10;'};
values(end + 1) = 14;
cases{end + 1} = {'if true', '  result = 1;', 'end; if true', '  result = result + 2;', 'end'};
values(end + 1) = 3;
cases{end + 1} = {'if false, result = 1; elseif true, result = 2; else, result = 3; end'};
values(end + 1) = 2;
cases{end + 1} = {'result = 0;', 'for k = 1:3, if k > 1, result = result + k; end; end', ...
  'result = result + 1;'};
values(end + 1) = 6;
cases{end + 1} = {'if false', '  result = 1;', 'elseif true, if true', ...
  '    result = 3;', '  end', 'else', '  result = 4;', 'end'};
values(end + 1) = 3;
cases{end + 1} = {'switch 2, case 1, result = 1; otherwise, result = 2; end', ...
  'result = result + 1;'};
values(end + 1) = 3;
cases{end + 1} = {'result = 0;', 'for k = 1:2', ...
  '  switch k, case 1, result = result + 1; otherwise, result = result + 2; end', ...
  '  result = result + 10;', 'end'};
values(end + 1) = 23;
cases{end + 1} = {'try, error(''fixture:expected'', ''expected''); catch e, result = 2; end', ...
  'result = result + 1;'};
values(end + 1) = 3;
cases{end + 1} = {'try, error(''fixture:expected'', ''expected''); catch e; result = 4; end', ...
  'result = result + 1;'};
values(end + 1) = 5;
cases{end + 1} = {'try, error(''fixture:expected'', ''expected''); catch, result = 6; end', ...
  'result = result + 1;'};
values(end + 1) = 7;
cases{end + 1} = {'try, error(''fixture:expected'', ''expected''); catch; result = 8; end', ...
  'result = result + 1;'};
values(end + 1) = 9;
cases{end + 1} = {'a = [1, 2]; c = {3, 4};', ...
  's = struct(''end'', 5, ''if'', 6); text = ''if end %{'';', ...
  'if true, a(end) = 3; result = a(end) + c{end} + s.end + s.if; end % end if', ...
  'asserts.isequal(text, ''if end %{'');'};
values(end + 1) = 18;
cases{end + 1} = {'a = [1, 2];', 'if true', '  result = a(...', '    end);', 'end'};
values(end + 1) = 2;
cases{end + 1} = {'while true, result = 1; break; end', 'result = result + 1;'};
values(end + 1) = 2;
cases{end + 1} = {'properties = 1; methods = 2; arguments = [1, 2];', ...
  'if true, events = 3; end', 'result = properties + methods + arguments(end) + events;'};
values(end + 1) = 8;
cases{end + 1} = {'result = smartindent_inline_local(3);', ...
  'function y = smartindent_inline_local(x)', '  arguments', '    x = 1', '  end', ...
  '  if x > 0, y = x; else, y = 0; end', 'end'};
values(end + 1) = 3;

% Every unformatted fixture must execute before testing the formatting operation.
files = cell(size(cases));
for k = 1:numel(cases)
  files{k} = fullfile(folder, ['inline_', int2str(k), '.m']);
  filewrite(files{k}, strjoin(strtrim(cases{k}), char(10)));
  executeFixture(files{k}, values(k));
end
for k = 1:numel(cases)
  checkFormatting(files{k}, cases{k}, values(k));
end

% Class methods have no ordinary lexer tokens, so their inline blocks use the fallback.
classFile = fullfile(folder, 'smartindent_inline_class.m');
classLines = {'classdef smartindent_inline_class', '  methods', ...
  '    function y = value(obj, x)', '      if x > 0, y = x; else, y = -x; end', ...
  '    end', '  end', 'end'};
filewrite(classFile, strjoin(strtrim(classLines), char(10)));
addpath(folder);
object = smartindent_inline_class();
asserts.isequal(object.value(-3), 3);
smartindent(classFile, 2, false, false);
asserts.isequal(normalizedText(classFile), strjoin(classLines, char(10)));
smartindent(classFile, 2, false, true);
formattedClass = normalizedText(classFile);
smartindent(classFile, 2, false, true);
asserts.isequal(normalizedText(classFile), formattedClass);
object = smartindent_inline_class();
asserts.isequal(object.value(4), 4);

% True imbalance still fails without modifying the file or creating a backup.
invalid = {'if true, result = 1;', 'result = 1; end', ...
  'if true, a = [1, 2]; result = a(end);'};
for k = 1:numel(invalid)
  filename = fullfile(folder, ['invalid_', int2str(k), '.m']);
  filewrite(filename, invalid{k});
  rejected = false;
  try
    smartindent(filename, 2, true, false);
  catch
    rejected = true;
  end
  asserts.istrue(rejected);
  asserts.isequal(normalizedText(filename), invalid{k});
  asserts.isfalse(isfile([filename, '.bak']));
end
clear cleanup;
asserts.isfalse(isfolder(folder));

function executeFixture(filename, expected)
  output = evalc('run(filename);');
  asserts.isequal(result, expected);
end

function checkFormatting(filename, lines, expectedValue)
  % Indent-only output and full-format leading levels are independently specified.
  expected = strjoin(lines, char(10));
  smartindent(filename, 2, false, false);
  asserts.isequal(normalizedText(filename), expected);
  executeFixture(filename, expectedValue);
  smartindent(filename, 2, false, false);
  asserts.isequal(normalizedText(filename), expected);
  smartindent(filename, 2, false, true);
  formatted = normalizedText(filename);
  actualLines = strsplit(formatted, char(10));
  asserts.isequal(numel(actualLines), numel(lines));
  for j = 1:numel(lines)
    asserts.isequal(numel(regexp(actualLines{j}, '^ *', 'match', 'once')), ...
      numel(regexp(lines{j}, '^ *', 'match', 'once')));
  end
  executeFixture(filename, expectedValue);
  smartindent(filename, 2, false, true);
  asserts.isequal(normalizedText(filename), formatted);
  % The same logical line boundaries must hold after CRLF normalization.
  filewrite(filename, strrep(expected, char(10), char([13, 10])));
  smartindent(filename, 2, false, false);
  asserts.isequal(normalizedText(filename), expected);
end

function text = normalizedText(filename)
  text = strrep(fileread(filename, 'char', 'native'), char([13, 10]), char(10));
end

function cleanupFixtureFolder(folder, originalPath)
  path(originalPath);
  rmdir(folder, 's');
end
