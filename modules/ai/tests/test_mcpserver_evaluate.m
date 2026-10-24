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
  '{"jsonrpc":"2.0","id":2,"method":"tools/call","params":{"name":"evaluate_nelson_code","arguments":{"code":"1+1"}}}';
  '{"jsonrpc":"2.0","id":3,"method":"tools/call","params":{"name":"evaluate_nelson_code","arguments":{"code":"error(''mcp expected error'')"}}}'
};
response = ai_mcp_request(requests);
asserts.isequal(response.status, 0);
asserts.isequal(response.command_output, '');
asserts.istrue(contains(response.output, '"id":2'));
asserts.istrue(contains(response.output, '"id":3'));
asserts.istrue(contains(response.output, 'ans'));
asserts.istrue(contains(response.output, '2'));
asserts.istrue(contains(response.output, 'mcp expected error'));
asserts.istrue(contains(response.output, '"structuredContent"'));
asserts.istrue(contains(response.output, '"isError":true'));
%=============================================================================
