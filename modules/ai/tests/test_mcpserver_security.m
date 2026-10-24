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
outside_file = [tempname(), '.m'];
text_file = [nelsonroot(), '/nelson-format-ignore.json'];
ignored_file = [nelsonroot(), '/modules/com_engine/examples/COM_excel.m'];
filewrite(outside_file, 'a=1;');

requests = {
  '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{}}';
  ['{"jsonrpc":"2.0","id":2,"method":"tools/call","params":{"name":"format_nelson_file","arguments":{"script_path":"', outside_file, '"}}}'];
  ['{"jsonrpc":"2.0","id":3,"method":"tools/call","params":{"name":"format_nelson_file","arguments":{"script_path":"', text_file, '"}}}'];
  ['{"jsonrpc":"2.0","id":4,"method":"tools/call","params":{"name":"format_nelson_file","arguments":{"script_path":"', ignored_file, '"}}}'];
  '{"jsonrpc":"2.0","id":5,"method":"tools/call","params":{"name":"create_nelson_plot","arguments":{"code":"plot(1:10)"}}}'
};
response = ai_mcp_request(requests, 30, '--nelson-display-mode=cli');
asserts.isequal(response.status, 0);
asserts.isequal(response.command_output, '');
asserts.istrue(contains(response.output, 'format_nelson_file refuses paths outside workspace root'));
asserts.istrue(contains(response.output, 'format_nelson_file expects a .m file'));
asserts.istrue(contains(response.output, 'File is listed in nelson-format-ignore.json'));
asserts.istrue(contains(response.output, 'create_nelson_plot is not available in cli display mode'));
asserts.istrue(contains(response.output, '"isError":true'));

if isfile(outside_file)
  rmfile(outside_file);
end
%=============================================================================
