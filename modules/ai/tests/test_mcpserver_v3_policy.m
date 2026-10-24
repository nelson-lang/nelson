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
test_dir = [tempdir(), 'nelson-ai-policy'];
if isdir(test_dir)
  rmdir(test_dir, 's');
end
mkdir(test_dir);
format_file = [test_dir, '/mcp_policy_format.m'];
filewrite(format_file, 'a=1;');

requests = {
  '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{}}';
  '{"jsonrpc":"2.0","id":2,"method":"tools/call","params":{"name":"evaluate_nelson_code","arguments":{"code":"1+1"}}}';
  ['{"jsonrpc":"2.0","id":3,"method":"tools/call","params":{"name":"format_nelson_file","arguments":{"script_path":"', format_file, '"}}}'];
  '{"jsonrpc":"2.0","id":4,"method":"tools/call","params":{"name":"get_nelson_version","arguments":{}}}'
};
response = ai_mcp_request(requests, 30, '--allow-execution=false --allow-format=false');
asserts.isequal(response.status, 0);
asserts.isequal(response.command_output, '');
asserts.istrue(contains(response.output, 'Execution tools are disabled by server policy'));
asserts.istrue(contains(response.output, 'Formatting tools are disabled by server policy'));
asserts.istrue(contains(response.output, '"allow_execution":false'));
asserts.istrue(contains(response.output, '"allow_format":false'));

if isdir(test_dir)
  rmdir(test_dir, 's');
end
%=============================================================================
