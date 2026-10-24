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
  '{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"get_nelson_workspace","arguments":{}}}';
  '{"jsonrpc":"2.0","id":2,"method":"tools/call","params":{"name":"evaluate_nelson_code","arguments":{"code":"disp(repmat(''x'', 1, 220))"}}}'
};

response = ai_mcp_request(requests, 60, '--max-output-characters=80');
asserts.isequal(response.status, 0);
asserts.isequal(response.command_output, '');

workspace_response = ai_mcp_response_by_id(response.output, 1);
evaluate_response = ai_mcp_response_by_id(response.output, 2);

asserts.isequal(workspace_response.result.structuredContent.success, true);
asserts.isequal(workspace_response.result.structuredContent.data.max_output_characters, 80);
asserts.isequal(evaluate_response.result.structuredContent.success, true);
asserts.isequal(evaluate_response.result.structuredContent.output_truncated, true);
asserts.isequal(evaluate_response.result.structuredContent.error_truncated, false);
asserts.istrue(length(evaluate_response.result.structuredContent.output) <= 80);
asserts.istrue(contains(evaluate_response.result.structuredContent.output, '<truncated>'));
%=============================================================================
