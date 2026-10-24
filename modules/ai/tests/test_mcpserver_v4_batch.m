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
  '[{"jsonrpc":"2.0","id":1,"method":"ping","params":{}},{"jsonrpc":"2.0","method":"initialized","params":{}},{"jsonrpc":"2.0","id":2,"method":"tools/call","params":{"name":"evaluate_nelson_code","arguments":{"code":"disp(''batch { braces }, comma'')"}}},{"jsonrpc":"2.0","id":3,"method":"tools/call","params":{"name":"unknown_tool","arguments":{}}}]'
};
response = ai_mcp_request(requests);
asserts.isequal(response.status, 0);
asserts.isequal(response.command_output, '');

ping_response = ai_mcp_response_by_id(response.output, 1);
eval_response = ai_mcp_response_by_id(response.output, 2);
unknown_response = ai_mcp_response_by_id(response.output, 3);
asserts.istrue(isstruct(ping_response));
asserts.istrue(isstruct(eval_response));
asserts.istrue(contains(eval_response.result.structuredContent.output, 'batch { braces }, comma'));
asserts.istrue(isstruct(unknown_response));
asserts.isequal(unknown_response.error.code, -32602);
asserts.istrue(contains(unknown_response.error.message, 'Unknown tool'));
%=============================================================================
