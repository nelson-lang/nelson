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
  '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"clientInfo":{"name":"nelson-test","version":"1"}}}';
  '{"jsonrpc":"2.0","method":"initialized","params":{}}';
  '{"jsonrpc":"2.0","id":2,"method":"tools/list","params":{}}';
  '{"jsonrpc":"2.0","id":3,"method":"resources/list","params":{}}';
  '{"jsonrpc":"2.0","id":4,"method":"resources/read","params":{"uri":"guidelines://nelson-coding"}}';
  '{"jsonrpc":"2.0","id":5,"method":"prompts/list","params":{}}'
};
response = ai_mcp_request(requests);
asserts.isequal(response.status, 0);
asserts.isequal(response.command_output, '');
asserts.istrue(contains(response.output, '"id":1'));
asserts.istrue(contains(response.output, '"id":2'));
asserts.istrue(contains(response.output, '"id":3'));
asserts.istrue(contains(response.output, '"id":4'));
asserts.istrue(contains(response.output, '"id":5'));
asserts.istrue(contains(response.output, 'evaluate_nelson_code'));
asserts.istrue(contains(response.output, 'create_nelson_plot'));
asserts.istrue(contains(response.output, 'get_nelson_workspace'));
asserts.istrue(contains(response.output, 'guidelines://nelson-coding'));
asserts.istrue(contains(response.output, 'guidelines://nelson-testing'));
asserts.istrue(contains(response.output, 'nelson-code-review'));
%=============================================================================
