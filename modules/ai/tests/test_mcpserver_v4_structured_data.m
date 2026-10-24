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
test_dir = strrep([tempdir(), 'nelson-ai-v4-structured'], '\', '/');
if isdir(test_dir)
  rmdir(test_dir, 's');
end
mkdir(test_dir);
test_file = [test_dir, '/test_mcp_v4_test_case.m'];
filewrite(test_file, 'asserts.isequal(3 * 3, 9);');

requests = {
  '{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"detect_nelson_modules","arguments":{"module":"ai"}}}';
  ['{"jsonrpc":"2.0","id":2,"method":"tools/call","params":{"name":"run_nelson_test_file","arguments":{"target":"', test_file, '"}}}'];
  '{"jsonrpc":"2.0","id":3,"method":"tools/call","params":{"name":"find_nelson_symbol","arguments":{"symbol":"mcpserver"}}}'
};
response = ai_mcp_request(requests, 120, ['--workspace-root=''', test_dir, '''']);
asserts.isequal(response.status, 0);
asserts.isequal(response.command_output, '');

modules_response = ai_mcp_response_by_id(response.output, 1);
test_response = ai_mcp_response_by_id(response.output, 2);
symbol_response = ai_mcp_response_by_id(response.output, 3);

asserts.istrue(isfield(modules_response.result.structuredContent.data, 'modules'));
asserts.istrue(numel(modules_response.result.structuredContent.data.modules) >= 1);
asserts.isequal(modules_response.result.structuredContent.data.modules.name, 'ai');
asserts.istrue(isfield(test_response.result.structuredContent.data, 'passed'));
asserts.isequal(test_response.result.structuredContent.data.failed, 0);
asserts.istrue(test_response.result.structuredContent.data.passed >= 1);
asserts.istrue(isfield(symbol_response.result.structuredContent.data, 'matches'));
% Symbol search anchors on source files (help XML, src, builtin). Installed
% builds ship only compiled help (*.nhz), so a builtin like mcpserver has no
% name-anchored file to match there; only assert matches on a source checkout.
if isfile([nelsonroot(), '/modules/ai/help/en_US/xml/mcpserver.xml'])
  asserts.istrue(numel(symbol_response.result.structuredContent.data.matches) >= 1);
end

if isdir(test_dir)
  rmdir(test_dir, 's');
end
%=============================================================================
