%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--SEQUENTIAL TEST REQUIRED-->
%=============================================================================
% format_nelson_file only accepts files under nelsonroot that are not listed
% in nelson-format-ignore.json (which excludes 'temp/'): use a dedicated
% folder under nelsonroot whose name matches no ignore pattern.
test_dir = [nelsonroot(), '/nelson-ai-mcp-run-check-format-fixture'];
[mkStatus, mkMsg] = mkdir(test_dir);
if mkStatus
  rmdir(test_dir, 's');
else
  skip_testsuite(true, 'nelsonroot is not writable (installed binary).');
end
if isdir(test_dir)
  rmdir(test_dir, 's');
end
mkdir(test_dir);
cleanup_test_dir = onCleanup(@() rmdir(test_dir, 's')); %#ok<NASGU>

script_file = [test_dir, '/mcp_run_file_case.m'];
test_file = [test_dir, '/mcp_test_file_case.m'];
format_file = [test_dir, '/mcp_format_case.m'];
format_config_file = [test_dir, '/nelson-format.json'];
filewrite(script_file, ['mcpRunFileValue = 41 + 1;', newline(), 'disp(mcpRunFileValue);']);
filewrite(test_file, 'asserts.isequal(2 + 2, 4);');
filewrite(format_file, ['if 1', newline(), 'a=1;', newline(), 'end', newline()]);
filewrite(format_config_file, ['{', newline(), ...
  '"indentSize": 4,', newline(), ...
  '"fullFormat": true,', newline(), ...
  '"includeSubfolders": false,', newline(), ...
  '"excludePathContains": []', newline(), ...
  '}']);

requests = {
  '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{}}';
  ['{"jsonrpc":"2.0","id":2,"method":"tools/call","params":{"name":"run_nelson_file","arguments":{"script_path":"', script_file, '"}}}'];
  ['{"jsonrpc":"2.0","id":3,"method":"tools/call","params":{"name":"run_nelson_test_file","arguments":{"target":"', test_file, '"}}}'];
  ['{"jsonrpc":"2.0","id":4,"method":"tools/call","params":{"name":"check_nelson_code","arguments":{"script_path":"', format_file, '"}}}'];
  ['{"jsonrpc":"2.0","id":5,"method":"tools/call","params":{"name":"format_nelson_file","arguments":{"script_path":"', format_file, '"}}}']
};
response = ai_mcp_request(requests);
asserts.isequal(response.status, 0);
asserts.isequal(response.command_output, '');
asserts.istrue(contains(response.output, '"id":2'));
asserts.istrue(contains(response.output, '"id":3'));
asserts.istrue(contains(response.output, '"id":4'));
asserts.istrue(contains(response.output, '"id":5'));
asserts.istrue(contains(response.output, '42'));
asserts.istrue(contains(response.output, '"tests":'));
asserts.istrue(contains(response.output, '"passed":'));
asserts.istrue(contains(response.output, '"failed":'));
asserts.istrue(contains(fileread(format_file), '    a = 1'));
%=============================================================================
