%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% tool call as a bare JSON object
[isTool, name, args] = nelson.ai.parseToolCall('{"tool": "evaluate_nelson_code", "arguments": {"code": "1+1"}}');
assert_istrue(isTool);
assert_isequal(name, 'evaluate_nelson_code');
assert_isequal(args.code, '1+1');
%=============================================================================
% tool call inside a json code fence, with surrounding prose
reply = sprintf('Let me run that.\n```json\n{"tool": "get_nelson_workspace", "arguments": {}}\n```\n');
[isTool, name] = nelson.ai.parseToolCall(reply);
assert_istrue(isTool);
assert_isequal(name, 'get_nelson_workspace');
%=============================================================================
% plain final answer is not a tool call
isTool = nelson.ai.parseToolCall('The result is 42.');
assert_isfalse(isTool);
%=============================================================================
% a JSON object without a tool field is not a tool call
isTool = nelson.ai.parseToolCall('{"final": "done"}');
assert_isfalse(isTool);
%=============================================================================
% nelson.ai.agent validates its prompt argument
assert_checkerror('nelson.ai.agent(42)', 'prompt must be a character vector or string scalar.', ...
  'Nelson:ai:promptMustBeText');
%=============================================================================
% the parsed tool call runs through nelson.ai.mcp.call end to end (no model involved)
[isTool, name, args] = nelson.ai.parseToolCall('{"tool": "evaluate_nelson_code", "arguments": {"code": "disp(6*7)"}}');
r = nelson.ai.mcp.call(name, args);
assert_istrue(r.success);
assert_istrue(contains(r.output, '42'));
%=============================================================================
