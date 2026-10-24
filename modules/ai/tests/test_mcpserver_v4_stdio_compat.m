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
requests = {
  '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"clientInfo":{"name":"compat-test","version":"1"}}}';
  '{"jsonrpc":"2.0","method":"initialized","params":{}}';
  '{"jsonrpc":"2.0","id":2,"method":"tools/list","params":{}}';
  '{"jsonrpc":"2.0","id":3,"method":"prompts/list","params":{}}';
  '{"jsonrpc":"2.0","id":4,"method":"resources/read","params":{"uri":"guidelines://nelson-testing"}}';
  '{"jsonrpc":"2.0","id":5,"method":"tools/call","params":{"name":"get_nelson_version","arguments":{}}}'
};
response = ai_mcp_request(requests);
asserts.isequal(response.status, 0);
asserts.isequal(response.command_output, '');

initialize_response = ai_mcp_response_by_id(response.output, 1);
tools_response = ai_mcp_response_by_id(response.output, 2);
prompts_response = ai_mcp_response_by_id(response.output, 3);
resource_response = ai_mcp_response_by_id(response.output, 4);
version_response = ai_mcp_response_by_id(response.output, 5);

asserts.istrue(isstruct(initialize_response));
asserts.isequal(initialize_response.result.serverInfo.name, 'nelson');
asserts.istrue(isfield(initialize_response.result.capabilities, 'tools'));
asserts.istrue(isfield(initialize_response.result.capabilities, 'resources'));
asserts.istrue(isfield(initialize_response.result.capabilities, 'prompts'));
asserts.istrue(any(strcmp({tools_response.result.tools.name}, 'evaluate_nelson_code')));
asserts.istrue(any(strcmp({prompts_response.result.prompts.name}, 'nelson-code-review')));
asserts.istrue(contains(resource_response.result.contents.text, 'test_run'));
asserts.isequal(version_response.result.structuredContent.success, true);
asserts.istrue(isfield(version_response.result.structuredContent.data, 'mcp_server_version'));
%=============================================================================
