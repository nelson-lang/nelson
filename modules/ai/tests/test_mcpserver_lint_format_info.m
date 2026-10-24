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
% Fixtures live outside nelsonroot: nelson-format-ignore.json excludes 'temp/'.
test_dir = strrep(tempname(), '\', '/');
if isdir(test_dir)
  rmdir(test_dir, 's');
end
mkdir(test_dir);

issue_file = [test_dir, '/mcp_lint_issue_case.m'];
ok_file = [test_dir, '/mcp_lint_ok_case.m'];
format_file = [test_dir, '/mcp_format_check_case.m'];
format_config_file = [test_dir, '/nelson-format.json'];
outside_file = [strrep(tempname(), '\', '/'), '_outside_lint.m'];
asserts.isfalse(startsWith(outside_file, [test_dir, '/']));

filewrite(issue_file, ['function y = mcp_lint_issue_case(x)', newline(), ...
  'y = x;   ', newline(), ...
  'end', newline()]);
filewrite(ok_file, ['function y = mcp_lint_ok_case(x)', newline(), ...
  'y = x;', newline(), ...
  'end', newline()]);
filewrite(format_file, ['if 1', newline(), 'a=1;', newline(), 'end', newline()]);
filewrite(format_config_file, ['{', newline(), ...
  '"indentSize": 4,', newline(), ...
  '"fullFormat": true,', newline(), ...
  '"includeSubfolders": false,', newline(), ...
  '"excludePathContains": []', newline(), ...
  '}']);
filewrite(outside_file, 'a = 1;');

workspace_option = ['--workspace-root=', test_dir];
requests = {
  '{"jsonrpc":"2.0","id":1,"method":"tools/list","params":{}}';
  ['{"jsonrpc":"2.0","id":2,"method":"tools/call","params":{"name":"lint_nelson_code","arguments":{"path":"', issue_file, '","format":"json"}}}'];
  ['{"jsonrpc":"2.0","id":3,"method":"tools/call","params":{"name":"lint_nelson_code","arguments":{"path":"', ok_file, '","format":"json"}}}'];
  ['{"jsonrpc":"2.0","id":4,"method":"tools/call","params":{"name":"check_nelson_format","arguments":{"path":"', format_file, '","config_path":"', format_config_file, '"}}}'];
  '{"jsonrpc":"2.0","id":5,"method":"tools/call","params":{"name":"get_nelson_function_info","arguments":{"symbol":"mcpserver"}}}';
  ['{"jsonrpc":"2.0","id":6,"method":"tools/call","params":{"name":"lint_nelson_code","arguments":{"path":"', outside_file, '"}}}']
};
response = ai_mcp_request(requests, 120, workspace_option);

asserts.isequal(response.status, 0);
asserts.isequal(response.command_output, '');
asserts.istrue(contains(response.output, 'lint_nelson_code'));
asserts.istrue(contains(response.output, 'check_nelson_format'));
asserts.istrue(contains(response.output, 'get_nelson_function_info'));

lint_issue_response = ai_mcp_response_by_id(response.output, 2);
asserts.isequal(lint_issue_response.result.structuredContent.success, true);
asserts.isequal(lint_issue_response.result.structuredContent.data.status, 1);
asserts.isequal(lint_issue_response.result.structuredContent.data.has_diagnostics, true);
asserts.istrue(contains(lint_issue_response.result.structuredContent.output, 'NLS0010'));

lint_ok_response = ai_mcp_response_by_id(response.output, 3);
asserts.isequal(lint_ok_response.result.structuredContent.success, true);
asserts.isequal(lint_ok_response.result.structuredContent.data.status, 0);
asserts.isequal(lint_ok_response.result.structuredContent.data.has_diagnostics, false);

format_response = ai_mcp_response_by_id(response.output, 4);
asserts.isequal(format_response.result.structuredContent.success, true);
asserts.isequal(format_response.result.structuredContent.data.status, 1);
asserts.isequal(format_response.result.structuredContent.data.would_format, true);
asserts.istrue(contains(format_response.result.structuredContent.output, 'would format'));
asserts.istrue(contains(fileread(format_file), 'a=1;'));
asserts.isfalse(contains(fileread(format_file), '    a = 1'));

info_response = ai_mcp_response_by_id(response.output, 5);
asserts.isequal(info_response.result.structuredContent.success, true);
asserts.isequal(info_response.result.structuredContent.data.symbol, 'mcpserver');
asserts.isfalse(isempty(info_response.result.structuredContent.data.which));
% has_tests/has_help are derived from a source-tree symbol search anchored on
% help XML. Installed builds ship only compiled help (*.nhz), so these flags are
% only meaningful on a source checkout.
if isfile([nelsonroot(), '/modules/ai/help/en_US/xml/mcpserver.xml'])
  asserts.istrue(info_response.result.structuredContent.data.has_tests);
  asserts.istrue(info_response.result.structuredContent.data.has_help);
end

outside_response = ai_mcp_response_by_id(response.output, 6);
asserts.isequal(outside_response.result.structuredContent.success, false);
asserts.istrue(contains(outside_response.result.structuredContent.error, 'outside workspace root'));
if isdir(test_dir)
  rmdir(test_dir, 's');
end
if isfile(outside_file)
  rmfile(outside_file);
end
%=============================================================================
