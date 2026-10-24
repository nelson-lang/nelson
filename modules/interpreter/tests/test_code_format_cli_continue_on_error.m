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
% nelson-format must report a file it cannot parse and keep processing the
% remaining files instead of stopping the traversal at the first failure.
workdir = tempname();
mkdir(workdir);
workdirCleanup = onCleanup(@() rmdir(workdir, 's')); %#ok<NASGU>
formatCommand = nelson_tool_command_for_test('nelson-format');
%=============================================================================
% Upper-case 'A' sorts before the lower-case valid file name.
invalidFile = [workdir, '/AAInvalidClassdef.m'];
invalidSource = ['classdef AAInvalidClassdef', char(10), ...
  '  properties (Access = package)', char(10), ...
  '    Value = 1', char(10), ...
  '  end', char(10), ...
  'end', char(10)];
filewrite(invalidFile, invalidSource);
validFile = [workdir, '/zz_badly_formatted.m'];
validSource = ['if true', char(10), 'a=1', char(10), 'end', char(10)];
filewrite(validFile, validSource);
%=============================================================================
[status, output] = system([formatCommand, ' --check "', workdir, '" 2>&1']);
asserts.isequal(status, 2);
asserts.istrue(contains(output, 'AAInvalidClassdef.m: Invalid classdef attribute value: Access = package'));
asserts.istrue(contains(output, 'would format'));
asserts.istrue(contains(output, 'zz_badly_formatted.m'));
%=============================================================================
[status, output] = system([formatCommand, ' --include-subfolders true --check "', workdir, '" 2>&1']);
asserts.isequal(status, 2);
asserts.istrue(contains(output, 'zz_badly_formatted.m'));
%=============================================================================
[status, output] = system([formatCommand, ' --check "', invalidFile, '" "', validFile, '" 2>&1']);
asserts.isequal(status, 2);
asserts.istrue(contains(output, 'zz_badly_formatted.m'));
%=============================================================================
[status, output] = system([formatCommand, ' "', workdir, '" 2>&1']);
asserts.isequal(status, 2);
asserts.istrue(contains(output, 'formatted: '));
asserts.istrue(contains(output, 'zz_badly_formatted.m'));
asserts.isequal(strrep(fileread(invalidFile, 'char', 'native'), char([13, 10]), char(10)), invalidSource);
formattedText = strrep(fileread(validFile, 'char', 'native'), char([13, 10]), char(10));
asserts.istrue(contains(formattedText, '  a = 1'));
%=============================================================================
