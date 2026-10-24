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
% nelson-format parses without an evaluator: invalid code must be reported
% (exit status 2) and left untouched, never crash or hang the formatter.
workdir = tempname();
mkdir(workdir);
workdirCleanup = onCleanup(@() rmdir(workdir, 's')); %#ok<NASGU>
formatCommand = nelson_tool_command_for_test('nelson-format');
%=============================================================================
parserErrorFile = [workdir, '/parser_error_case.m'];
parserErrorSource = ['B = single([(1:4)', char(39), '(5:8)', char(39), ']);', char(10)];
filewrite(parserErrorFile, parserErrorSource);
[status, output] = system([formatCommand, ' "', parserErrorFile, '"']);
asserts.isequal(status, 2);
asserts.istrue(contains(output, 'Expecting'));
asserts.isequal(strrep(fileread(parserErrorFile, 'char', 'native'), char([13, 10]), char(10)), parserErrorSource);
%=============================================================================
lexerErrorFile = [workdir, '/lexer_error_case.m'];
lexerErrorSource = ['x = 1.8e + 99;', char(10)];
filewrite(lexerErrorFile, lexerErrorSource);
[status, output] = system([formatCommand, ' --check "', lexerErrorFile, '"']);
asserts.isequal(status, 2);
asserts.istrue(contains(output, 'malformed floating point constant'));
asserts.isequal(strrep(fileread(lexerErrorFile, 'char', 'native'), char([13, 10]), char(10)), lexerErrorSource);
%=============================================================================
