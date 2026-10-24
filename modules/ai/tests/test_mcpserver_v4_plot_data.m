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
% <--WITH DISPLAY-->
%=============================================================================
plot_dir = [tempdir(), 'nelson-ai'];
if isdir(plot_dir)
  rmdir(plot_dir, 's');
end

requests = {
  '{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"create_nelson_plot","arguments":{"code":"plot(1:4)"}}}'
};
response = ai_mcp_request(requests, 60);
asserts.isequal(response.status, 0);
asserts.isequal(response.command_output, '');

plot_response = ai_mcp_response_by_id(response.output, 1);
plot_data = plot_response.result.structuredContent.data;
asserts.isequal(plot_data.mimeType, 'image/png');
asserts.istrue(plot_data.size > 0);
asserts.istrue(isfile(plot_data.path));

if isdir(plot_dir)
  rmdir(plot_dir, 's');
end
%=============================================================================
