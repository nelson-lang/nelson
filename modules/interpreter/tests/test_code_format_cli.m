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
workdir = tempname();
mkdir(workdir);
formatCommand = nelson_tool_command_for_test('nelson-format');

formatFile = [workdir, '/format_case.m'];
filewrite(formatFile, ['function y = format_case(a,b)', newline(), ...
  'y=a+b', newline(), ...
  'end']);

[status, output] = system([formatCommand, ' --check "', formatFile, '"']);
asserts.isequal(status, 1);
asserts.istrue(contains(output, 'format_case.m'));

[status, output] = system([formatCommand, ' "', formatFile, '"']);
asserts.isequal(status, 0);
asserts.istrue(contains(output, 'format_case.m'));
formattedText = strrep(fileread(formatFile, 'char', 'native'), char([13, 10]), char(10));
asserts.istrue(contains(formattedText, 'function y = format_case(a, b)'));
asserts.istrue(contains(formattedText, '  y = a + b'));

[status, output] = system([formatCommand, ' --check "', formatFile, '"']);
asserts.isequal(status, 0);
asserts.isequal(output, '');

propertiesAssignmentFile = [workdir, '/format_properties_assignment.m'];
propertiesAssignmentInput = ['properties = 1;', newline(), ...
  'asserts.isequal(properties, 1);'];
filewrite(propertiesAssignmentFile, propertiesAssignmentInput);
[status, output] = system([formatCommand, ' --full-format false --check "', propertiesAssignmentFile, '"']);
asserts.isequal(status, 0);
asserts.isequal(output, '');

filewrite(propertiesAssignmentFile, ['properties=1;', newline(), ...
  'asserts.isequal(properties,1);']);
[status, output] = system([formatCommand, ' --full-format true "', propertiesAssignmentFile, '"']);
asserts.isequal(status, 0);
asserts.istrue(contains(output, 'format_properties_assignment.m'));
propertiesAssignmentFormatted = strrep(fileread(propertiesAssignmentFile, 'char', 'native'), char([13, 10]), char(10));
asserts.isequal(propertiesAssignmentFormatted, propertiesAssignmentInput);

[status, output] = system([formatCommand, ' --full-format true "', propertiesAssignmentFile, '"']);
asserts.isequal(status, 0);
asserts.isequal(output, '');
propertiesAssignmentFormattedAgain = strrep(fileread(propertiesAssignmentFile, 'char', 'native'), char([13, 10]), char(10));
asserts.isequal(propertiesAssignmentFormattedAgain, propertiesAssignmentFormatted);
run(propertiesAssignmentFile);

filewrite([workdir, '/FormatPropertiesBlockProbe.m'], ['classdef FormatPropertiesBlockProbe', newline(), ...
  'properties(Access=private)', newline(), ...
  'value=41;', newline(), ...
  'properties=1;', newline(), ...
  'end', newline(), ...
  'methods', newline(), ...
  'function y = read(obj)', newline(), ...
  'properties=[obj.properties 2];', newline(), ...
  'properties(1)=properties(1)+1;', newline(), ...
  'y=obj.value+properties(1)-1;', newline(), ...
  'end', newline(), ...
  'end', newline(), ...
  'end']);

propertiesBlockFile = [workdir, '/FormatPropertiesBlockProbe.m'];
[status, output] = system([formatCommand, ' --full-format true "', propertiesBlockFile, '"']);
asserts.isequal(status, 0);
asserts.istrue(contains(output, 'FormatPropertiesBlockProbe.m'));
propertiesBlockFormatted = strrep(fileread(propertiesBlockFile, 'char', 'native'), char([13, 10]), char(10));
asserts.istrue(contains(propertiesBlockFormatted, '  properties (Access = private)'));
asserts.istrue(contains(propertiesBlockFormatted, ['  properties (Access = private)', char(10), '    value = 41;']));

[status, output] = system([formatCommand, ' --full-format true "', propertiesBlockFile, '"']);
asserts.isequal(status, 0);
asserts.isequal(output, '');
propertiesBlockFormattedAgain = strrep(fileread(propertiesBlockFile, 'char', 'native'), char([13, 10]), char(10));
asserts.isequal(propertiesBlockFormattedAgain, propertiesBlockFormatted);

addpath(workdir);
propertiesBlockProbe = FormatPropertiesBlockProbe();
asserts.isequal(propertiesBlockProbe.read(), 42);

mkdir([workdir, '/+formatfixture']);
filewrite([workdir, '/+formatfixture/convert.m'], ['function y = convert(x, fromUnit, toUnit)', newline(), ...
  'if strcmp(fromUnit, ''m'') && strcmp(toUnit, ''mm'')', newline(), ...
  'y = x * 1000;', newline(), ...
  'else', newline(), ...
  'y = x;', newline(), ...
  'end', newline(), ...
  'end']);

filewrite([workdir, '/DotMethodProbe.m'], ['classdef DotMethodProbe', newline(), ...
  'methods', newline(), ...
  'function y = scale(obj, x)', newline(), ...
  'y = x * 2;', newline(), ...
  'end', newline(), ...
  'end', newline(), ...
  'end']);
rehash();

dotFormatFile = [workdir, '/format_dot_case.m'];
filewrite(dotFormatFile, ['function y = format_dot_case(obj,x,fieldName)', newline(), ...
  'a=formatfixture.convert(x.value,''m'',''mm'');', newline(), ...
  'b=obj.scale(x.(fieldName));', newline(), ...
  'c=(1:3).^2;', newline(), ...
  'd=[1 2 3].*2;', newline(), ...
  'e=[2 4 8]./2;', newline(), ...
  'f=[2 4 8].\16;', newline(), ...
  'g=x.value'';', newline(), ...
  'h=x.value.'';', newline(), ...
  'y=a+b+c(2)+d(3)+e(1)+f(1)+g+h;', newline(), ...
  'end']);

[status, output] = system([formatCommand, ' --full-format true "', dotFormatFile, '"']);
asserts.isequal(status, 0);
asserts.istrue(contains(output, 'format_dot_case.m'));
dotFormattedText = strrep(fileread(dotFormatFile, 'char', 'native'), char([13, 10]), char(10));
asserts.istrue(contains(dotFormattedText, 'formatfixture.convert(x.value, ''m'', ''mm'')'));
asserts.istrue(contains(dotFormattedText, 'obj.scale(x.(fieldName))'));
asserts.istrue(contains(dotFormattedText, '(1:3) .^ 2'));
asserts.istrue(contains(dotFormattedText, '[1 2 3] .* 2'));
asserts.istrue(contains(dotFormattedText, '[2 4 8] ./ 2'));
asserts.istrue(contains(dotFormattedText, '[2 4 8] .\ 16'));
asserts.istrue(contains(dotFormattedText, 'x.value'''));
asserts.istrue(contains(dotFormattedText, 'x.value.'''));
asserts.isfalse(contains(dotFormattedText, ' . '));

dotFormattedOnce = dotFormattedText;
[status, output] = system([formatCommand, ' --full-format true "', dotFormatFile, '"']);
asserts.isequal(status, 0);
asserts.isequal(output, '');
dotFormattedTwice = strrep(fileread(dotFormatFile, 'char', 'native'), char([13, 10]), char(10));
asserts.isequal(dotFormattedTwice, dotFormattedOnce);

rehash();
dotInput = struct();
dotInput.value = 3;
dotResult = format_dot_case(DotMethodProbe(), dotInput, 'value');
asserts.isequal(dotResult, 3031);

[status, output] = system([formatCommand, ' "', workdir, '/missing_case.m"']);
asserts.isequal(status, 2);
asserts.istrue(contains(output, 'path does not exist'));

invalidFile = [workdir, '/invalid_case.m'];
invalidInput = ['if true', newline(), 'disp(''missing end'')'];
filewrite(invalidFile, invalidInput);
[status, output] = system([formatCommand, ' "', invalidFile, '"']);
asserts.isequal(status, 2);
asserts.istrue(contains(output, 'Missing ''end'''));
asserts.isequal(strrep(fileread(invalidFile, 'char', 'native'), char([13, 10]), char(10)), invalidInput);

recursiveDir = [workdir, '/recursive'];
mkdir(recursiveDir);
mkdir([recursiveDir, '/sub']);
topFile = [recursiveDir, '/top_case.m'];
nestedFile = [recursiveDir, '/sub/nested_case.m'];
filewrite(topFile, ['if true', newline(), 'a=1', newline(), 'end']);
filewrite(nestedFile, ['if true', newline(), 'b=2', newline(), 'end']);

[status, output] = system([formatCommand, ' --check "', recursiveDir, '"']);
asserts.isequal(status, 1);
asserts.istrue(contains(output, 'top_case.m'));
asserts.isfalse(contains(output, 'nested_case.m'));

[status, output] = system([formatCommand, ' --include-subfolders true --check "', recursiveDir, '"']);
asserts.isequal(status, 1);
asserts.istrue(contains(output, 'top_case.m'));
asserts.istrue(contains(output, 'nested_case.m'));

configFile = [workdir, '/nelson-format-custom.json'];
filewrite(configFile, ['{', newline(), ...
  '"indentSize": 4,', newline(), ...
  '"fullFormat": true,', newline(), ...
  '"includeSubfolders": false,', newline(), ...
  '"excludePathContains": []', newline(), ...
  '}']);
configCase = [workdir, '/config_case.m'];
filewrite(configCase, ['if true', newline(), 'x=1', newline(), 'end']);
[status, output] = system([formatCommand, ' --config "', configFile, '" "', configCase, '"']);
asserts.isequal(status, 0);
asserts.istrue(contains(output, 'config_case.m'));
configText = strrep(fileread(configCase, 'char', 'native'), char([13, 10]), char(10));
asserts.istrue(contains(configText, ['if true', char(10), '    x = 1']));

filewrite(configCase, ['if true', newline(), 'x=1', newline(), 'end']);
[status, output] = system([formatCommand, ' --config "', configFile, '" --indent-size 2 "', configCase, '"']);
asserts.isequal(status, 0);
configOverrideText = strrep(fileread(configCase, 'char', 'native'), char([13, 10]), char(10));
asserts.istrue(contains(configOverrideText, ['if true', char(10), '  x = 1']));

autoConfigDir = [workdir, '/auto_config'];
mkdir(autoConfigDir);
mkdir([autoConfigDir, '/sub']);
filewrite([autoConfigDir, '/nelson-format.json'], ['{', newline(), ...
  '"includeSubfolders": true', newline(), ...
  '}']);
autoTopFile = [autoConfigDir, '/auto_top.m'];
autoNestedFile = [autoConfigDir, '/sub/auto_nested.m'];
filewrite(autoTopFile, ['if true', newline(), 'a=1', newline(), 'end']);
filewrite(autoNestedFile, ['if true', newline(), 'b=2', newline(), 'end']);
[status, output] = system([formatCommand, ' --check "', autoConfigDir, '"']);
asserts.isequal(status, 1);
asserts.istrue(contains(output, 'auto_top.m'));
asserts.istrue(contains(output, 'auto_nested.m'));

excludeConfigFile = [workdir, '/nelson-format-exclude.json'];
filewrite(excludeConfigFile, ['{', newline(), ...
  '"excludePathContains": ["config_ignore_case.m"]', newline(), ...
  '}']);
configIgnoredFile = [workdir, '/config_ignore_case.m'];
filewrite(configIgnoredFile, ['if true', newline(), 'ignored=1', newline(), 'end']);
[status, output] = system([formatCommand, ' --config "', excludeConfigFile, '" --check "', configIgnoredFile, '"']);
asserts.isequal(status, 0);
asserts.isequal(output, '');

invalidConfigFile = [workdir, '/nelson-format-invalid.json'];
filewrite(invalidConfigFile, '{');
[status, output] = system([formatCommand, ' --config "', invalidConfigFile, '" "', configCase, '"']);
asserts.isequal(status, 2);
asserts.istrue(contains(output, 'failed to parse configuration file'));

invalidTypeConfigFile = [workdir, '/nelson-format-invalid-type.json'];
filewrite(invalidTypeConfigFile, '{"indentSize": "2"}');
[status, output] = system([formatCommand, ' --config "', invalidTypeConfigFile, '" "', configCase, '"']);
asserts.isequal(status, 2);
asserts.istrue(contains(output, 'indentSize must be a non-negative integer'));

unknownKeyConfigFile = [workdir, '/nelson-format-unknown-key.json'];
filewrite(unknownKeyConfigFile, '{"indent_size": 2}');
[status, output] = system([formatCommand, ' --config "', unknownKeyConfigFile, '" "', configCase, '"']);
asserts.isequal(status, 2);
asserts.istrue(contains(output, 'unknown configuration key'));

ignoredFile = [nelsonroot(), '/modules/spreadsheet/tests/test_writematrix.m'];
[status, output] = system([formatCommand, ' --check "', ignoredFile, '"']);
asserts.isequal(status, 0);
asserts.isequal(output, '');
%=============================================================================
