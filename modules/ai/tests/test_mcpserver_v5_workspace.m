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
workspace_dir = [tempdir(), 'nelson-ai-v5-workspace'];
outside_file = [tempdir(), 'nelson-ai-v5-outside.m'];
inside_file = [workspace_dir, '/inside_run.m'];

if isdir(workspace_dir)
  rmdir(workspace_dir, 's');
end
if isfile(outside_file)
  rmfile(outside_file);
end
mkdir(workspace_dir);
filewrite(inside_file, 'disp(321);');
filewrite(outside_file, 'disp(999);');

workspace_option = ['--workspace-root=''', strrep(workspace_dir, '\', '/'), ''''];
outside_json_path = strrep(outside_file, '\', '/');

requests = {
  '{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"get_nelson_workspace","arguments":{}}}';
  '{"jsonrpc":"2.0","id":2,"method":"tools/call","params":{"name":"run_nelson_file","arguments":{"script_path":"inside_run.m"}}}';
  ['{"jsonrpc":"2.0","id":3,"method":"tools/call","params":{"name":"check_nelson_code","arguments":{"script_path":"', outside_json_path, '"}}}'];
  ['{"jsonrpc":"2.0","id":4,"method":"tools/call","params":{"name":"format_nelson_file","arguments":{"script_path":"', outside_json_path, '"}}}'];
  ['{"jsonrpc":"2.0","id":5,"method":"tools/call","params":{"name":"evaluate_nelson_code","arguments":{"code":"disp(1)","project_path":"', strrep([tempdir()], '\', '/'), '"}}}']
};

response = ai_mcp_request(requests, 60, workspace_option);
asserts.isequal(response.status, 0);
asserts.isequal(response.command_output, '');

workspace_response = ai_mcp_response_by_id(response.output, 1);
run_response = ai_mcp_response_by_id(response.output, 2);
check_response = ai_mcp_response_by_id(response.output, 3);
format_response = ai_mcp_response_by_id(response.output, 4);
evaluate_response = ai_mcp_response_by_id(response.output, 5);

asserts.isequal(workspace_response.result.structuredContent.success, true);
asserts.istrue(contains(workspace_response.result.structuredContent.data.workspace_root, 'nelson-ai-v5-workspace'));
asserts.isequal(workspace_response.result.structuredContent.data.allow_execution, true);
asserts.isequal(workspace_response.result.structuredContent.data.allow_format, true);
asserts.isequal(run_response.result.structuredContent.success, true);
asserts.istrue(contains(run_response.result.structuredContent.output, '321'));
asserts.isequal(check_response.result.structuredContent.success, false);
asserts.istrue(contains(check_response.result.structuredContent.error, 'outside workspace root'));
asserts.isequal(format_response.result.structuredContent.success, false);
asserts.istrue(contains(format_response.result.structuredContent.error, 'outside workspace root'));
asserts.isequal(evaluate_response.result.structuredContent.success, false);
asserts.istrue(contains(evaluate_response.result.structuredContent.error, 'outside workspace root'));

if isfile(outside_file)
  rmfile(outside_file);
end
if isdir(workspace_dir)
  rmdir(workspace_dir, 's');
end
%=============================================================================
