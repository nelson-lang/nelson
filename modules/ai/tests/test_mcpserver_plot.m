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
  '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{}}';
  '{"jsonrpc":"2.0","id":2,"method":"tools/call","params":{"name":"create_nelson_plot","arguments":{"code":"plot(1:10)"}}}'
};
response = ai_mcp_request(requests, 60);
asserts.isequal(response.status, 0);
asserts.isequal(response.command_output, '');
asserts.istrue(contains(response.output, '"id":2'));
asserts.istrue(contains(response.output, '.png'));
asserts.istrue(contains(response.output, '"mimeType":"image/png"'));
asserts.istrue(contains(response.output, '"size":'));
asserts.istrue(isdir(plot_dir));
png_files = dir([plot_dir, '/*.png']);
asserts.istrue(length(png_files) >= 1);
if isdir(plot_dir)
  rmdir(plot_dir, 's');
end
%=============================================================================
