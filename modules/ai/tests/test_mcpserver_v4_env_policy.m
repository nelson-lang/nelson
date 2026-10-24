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
test_dir = [tempdir(), 'nelson-ai-env-policy'];
if isdir(test_dir)
  rmdir(test_dir, 's');
end
mkdir(test_dir);
script_file = [test_dir, '/mcp_env_run.m'];
format_file = [test_dir, '/mcp_env_format.m'];
filewrite(script_file, 'disp(42);');
filewrite(format_file, 'a=1;');

previousExecution = getenv('NELSON_MCP_SERVER_ALLOW_EXECUTION');
previousFormat = getenv('NELSON_MCP_SERVER_ALLOW_FORMAT');
setenv('NELSON_MCP_SERVER_ALLOW_EXECUTION', 'false');
setenv('NELSON_MCP_SERVER_ALLOW_FORMAT', 'false');

requests = {
  ['{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"run_nelson_file","arguments":{"script_path":"', script_file, '"}}}'];
  '{"jsonrpc":"2.0","id":2,"method":"tools/call","params":{"name":"run_nelson_test_file","arguments":{"target":"ai"}}}';
  '{"jsonrpc":"2.0","id":3,"method":"tools/call","params":{"name":"create_nelson_plot","arguments":{"code":"plot(1:3)"}}}';
  ['{"jsonrpc":"2.0","id":4,"method":"tools/call","params":{"name":"format_nelson_file","arguments":{"script_path":"', format_file, '"}}}']
};
response = ai_mcp_request(requests, 30);
asserts.isequal(response.status, 0);
asserts.isequal(response.command_output, '');
asserts.istrue(contains(response.output, 'Execution tools are disabled by server policy'));
asserts.istrue(contains(response.output, 'Formatting tools are disabled by server policy'));
asserts.isfalse(contains(fileread(format_file), 'a = 1'));
setenv('NELSON_MCP_SERVER_ALLOW_EXECUTION', previousExecution);
setenv('NELSON_MCP_SERVER_ALLOW_FORMAT', previousFormat);

if isdir(test_dir)
  rmdir(test_dir, 's');
end
%=============================================================================
