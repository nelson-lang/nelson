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
  '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{}}';
  '{"jsonrpc":"2.0","id":2,"method":"prompts/list","params":{}}';
  '{"jsonrpc":"2.0","id":3,"method":"prompts/get","params":{"name":"nelson-code-review","arguments":{"task":"inspect ai module"}}}';
  '{"jsonrpc":"2.0","id":4,"method":"tools/call","params":{"name":"evaluate_nelson_code","arguments":{"code":"1+1"}}}';
  '{"jsonrpc":"2.0","id":5,"method":"resources/read","params":{"uri":"guidelines://nelson-modules"}}'
};
response = ai_mcp_request(requests);
asserts.isequal(response.status, 0);
asserts.isequal(response.command_output, '');
asserts.istrue(contains(response.output, '"prompts"'));
asserts.istrue(contains(response.output, 'nelson-code-review'));
asserts.istrue(contains(response.output, 'inspect ai module'));
asserts.istrue(contains(response.output, '"structuredContent"'));
asserts.istrue(contains(response.output, 'guidelines://nelson-modules'));
% The module guideline excerpts a source-tree file; installed builds ship only
% compiled help (*.nhz), so this excerpt is only present on a source checkout.
if isfile([nelsonroot(), '/modules/ai/CMakeLists.txt'])
  asserts.istrue(contains(response.output, 'modules/ai/CMakeLists.txt'));
end
%=============================================================================
