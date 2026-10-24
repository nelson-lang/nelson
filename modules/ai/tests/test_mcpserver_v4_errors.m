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
  '{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"arguments":{}}}';
  '{"jsonrpc":"2.0","id":2,"method":"tools/call","params":{"name":"does_not_exist","arguments":{}}}';
  '{"jsonrpc":"2.0","id":3,"method":"resources/read","params":{"uri":"guidelines://unknown"}}';
  '{"jsonrpc":"2.0","id":4,"method":"prompts/get","params":{"name":"unknown-prompt","arguments":{}}}'
};
response = ai_mcp_request(requests);
asserts.isequal(response.status, 0);
asserts.isequal(response.command_output, '');
asserts.isequal(ai_mcp_response_by_id(response.output, 1).error.code, -32602);
asserts.istrue(contains(ai_mcp_response_by_id(response.output, 1).error.message, 'Tool name is required'));
asserts.isequal(ai_mcp_response_by_id(response.output, 2).error.code, -32602);
asserts.istrue(contains(ai_mcp_response_by_id(response.output, 2).error.message, 'Unknown tool'));
asserts.isequal(ai_mcp_response_by_id(response.output, 3).error.code, -32602);
asserts.istrue(contains(ai_mcp_response_by_id(response.output, 3).error.message, 'Unknown resource URI'));
asserts.isequal(ai_mcp_response_by_id(response.output, 4).error.code, -32602);
asserts.istrue(contains(ai_mcp_response_by_id(response.output, 4).error.message, 'Unknown prompt'));
%=============================================================================
