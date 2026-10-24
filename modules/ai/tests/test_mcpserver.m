%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
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
  '{"jsonrpc":"2.0","id":2,"method":"tools/list","params":{}}'
};
%=============================================================================
response = ai_mcp_request(requests);
asserts.isequal(response.status, 0);
asserts.isequal(response.command_output, '');
asserts.istrue(contains(response.output, '"jsonrpc":"2.0"'));
asserts.istrue(contains(response.output, '"id":1'));
asserts.istrue(contains(response.output, '"id":2'));
asserts.istrue(contains(response.output, 'evaluate_nelson_code'));
%=============================================================================
