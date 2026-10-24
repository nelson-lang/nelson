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
  '{"jsonrpc":"1.0","id":1,"method":"ping","params":{}}';
  '{"jsonrpc":"2.0","id":2,"params":{}}';
  '[{"jsonrpc":"2.0","id":3,"method":"ping","params":{}},{"jsonrpc":"2.0","id":4,"method":"tools/list","params":{}}]';
  '[]'
};
response = ai_mcp_request(requests);
asserts.isequal(response.status, 0);
asserts.isequal(response.command_output, '');
asserts.istrue(contains(response.output, '"id":1'));
asserts.istrue(contains(response.output, 'jsonrpc must be 2.0'));
asserts.istrue(contains(response.output, '"id":2'));
asserts.istrue(contains(response.output, 'method is required'));
asserts.istrue(contains(response.output, '"id":3'));
asserts.istrue(contains(response.output, '"id":4'));
asserts.istrue(contains(response.output, 'Batch request must not be empty'));
%=============================================================================
