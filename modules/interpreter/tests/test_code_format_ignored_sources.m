%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ENGLISH IMPOSED-->
% Sources sharing the .m extension without being Nelson code (Objective-C of
% the vendored WebUI library) and the classdef fixtures that are invalid on
% purpose are listed in nelson-format-ignore.json: nelson-format must skip them
% instead of reporting a parse error.
skip_testsuite(~isfile([nelsonroot(), '/nelson-format-ignore.json']), ...
  'nelson-format-ignore.json is not part of this installation.');
formatCommand = nelson_tool_command_for_test('nelson-format');
sources = {'modules/webview/src/webui/src/webview/macos_render_html.m', ...
  'modules/webview/src/webui/src/webview/wkwebview.m', ...
  'modules/interpreter/tests/classdef_syntax/invalid'};
checked = 0;
for k = 1:numel(sources)
  source = [nelsonroot(), '/', sources{k}];
  if ~isfile(source) && ~isdir(source)
    continue
  end
  [status, output] = system([formatCommand, ' --check "', source, '"']);
  asserts.isequal(status, 0);
  asserts.isequal(output, '');
  checked = checked + 1;
end
if checked == 0
  skip_testsuite(true, 'These sources are not part of this installation.');
end
%=============================================================================
