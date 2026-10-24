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
  '{"jsonrpc":"2.0","id":2,"method":"tools/list","params":{}}';
  '{"jsonrpc":"2.0","id":3,"method":"resources/list","params":{}}';
  '{"jsonrpc":"2.0","id":4,"method":"prompts/list","params":{}}';
  '{"jsonrpc":"2.0","id":5,"method":"resources/read","params":{"uri":"skills://nelson-core/testing"}}';
  '{"jsonrpc":"2.0","id":6,"method":"resources/read","params":{"uri":"skills://nelson-core/debugging"}}';
  '{"jsonrpc":"2.0","id":7,"method":"resources/read","params":{"uri":"skills://nelson-core/code-review"}}';
  '{"jsonrpc":"2.0","id":8,"method":"resources/read","params":{"uri":"skills://nflow/create-block"}}';
  '{"jsonrpc":"2.0","id":9,"method":"resources/read","params":{"uri":"skills://nflow/create-library"}}';
  '{"jsonrpc":"2.0","id":10,"method":"resources/read","params":{"uri":"skills://nflow/create-model"}}'
};
response = ai_mcp_request(requests);
asserts.isequal(response.status, 0);
asserts.isequal(response.command_output, '');
asserts.istrue(contains(response.output, '"id":2'));
asserts.istrue(contains(response.output, '"id":3'));
asserts.istrue(contains(response.output, '"id":4'));
asserts.istrue(contains(response.output, 'evaluate_nelson_code'));
asserts.istrue(contains(response.output, 'get_nelson_version'));
asserts.istrue(contains(response.output, 'guidelines://nelson-coding'));
asserts.istrue(contains(response.output, 'skills://nelson/catalog'));
asserts.istrue(contains(response.output, 'skills://nelson-core/packaging'));
asserts.istrue(contains(response.output, 'skills://nelson-data/analysis-workflows'));
asserts.istrue(contains(response.output, 'skills://nflow/create-block'));
asserts.istrue(contains(response.output, 'skills://nflow/create-library'));
asserts.istrue(contains(response.output, 'skills://nflow/create-model'));
asserts.istrue(contains(response.output, 'Nelson Core Testing'));
asserts.istrue(contains(response.output, 'Nelson Core Debugging'));
asserts.istrue(contains(response.output, 'Create an NFlow Block'));
asserts.istrue(contains(response.output, 'Create an NFlow Library'));
asserts.istrue(contains(response.output, 'Create an NFlow Model'));
asserts.istrue(contains(response.output, 'nflow-create-block'));
asserts.istrue(contains(response.output, 'nflow-create-library'));
asserts.istrue(contains(response.output, 'nflow-create-model'));
asserts.istrue(contains(response.output, 'Nelson .m Coding Standards'));
asserts.istrue(contains(response.output, 'Function Interface Standards'));
asserts.istrue(contains(response.output, 'nelson-code-review'));
%=============================================================================
