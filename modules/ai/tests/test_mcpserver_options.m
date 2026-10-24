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
work_dir = [tempdir(), 'nelson-ai-options'];
if isdir(work_dir)
  rmdir(work_dir, 's');
end
mkdir(work_dir);
cleanup_work_dir = onCleanup(@() rmdir(work_dir, 's')); %#ok<NASGU>

requests = {
  '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{}}';
  '{"jsonrpc":"2.0","id":2,"method":"tools/call","params":{"name":"evaluate_nelson_code","arguments":{"code":"pwd()"}}}'
};
response = ai_mcp_request(requests, 30, ['--initial-working-folder=''', work_dir, '''']);
asserts.isequal(response.status, 0);
asserts.isequal(response.command_output, '');
asserts.istrue(contains(response.output, 'nelson-ai-options'));

space_work_dir = [tempdir(), 'nelson-ai-options space'];
if isdir(space_work_dir)
  rmdir(space_work_dir, 's');
end
mkdir(space_work_dir);
cleanup_space_work_dir = onCleanup(@() rmdir(space_work_dir, 's')); %#ok<NASGU>
requests = {
  '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{}}';
  '{"jsonrpc":"2.0","id":2,"method":"tools/call","params":{"name":"get_nelson_workspace","arguments":{}}}'
};
response = ai_mcp_request(requests, 30, ['--workspace-root=''', space_work_dir, '''']);
asserts.isequal(response.status, 0);
asserts.isequal(response.command_output, '');
asserts.istrue(contains(response.output, 'nelson-ai-options space'));

previousDisplayMode = getenv('NELSON_MCP_SERVER_NELSON_DISPLAY_MODE');
cleanup_env = onCleanup(@() setenv('NELSON_MCP_SERVER_NELSON_DISPLAY_MODE', previousDisplayMode));
setenv('NELSON_MCP_SERVER_NELSON_DISPLAY_MODE', 'cli');
requests = {
  '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{}}';
  '{"jsonrpc":"2.0","id":2,"method":"tools/call","params":{"name":"get_nelson_version","arguments":{}}}'
};
response = ai_mcp_request(requests);
asserts.isequal(response.status, 0);
asserts.isequal(response.command_output, '');
asserts.istrue(contains(response.output, 'display_mode'));
asserts.istrue(contains(response.output, 'cli'));
%=============================================================================
