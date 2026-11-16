%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% nelson.ai.mcp.call invokes the Nelson tools in-process, no stdio transport
r = nelson.ai.mcp.call('get_nelson_version');
assert_isequal(class(r), 'struct');
assert_istrue(isfield(r, 'success'));
assert_istrue(isfield(r, 'output'));
assert_istrue(isfield(r, 'error'));
assert_istrue(isfield(r, 'data'));
assert_istrue(isfield(r, 'duration'));
assert_istrue(r.success);
%=============================================================================
r = nelson.ai.mcp.call('evaluate_nelson_code', struct('code', 'disp(2 + 3)'));
assert_istrue(r.success);
assert_istrue(contains(r.output, '5'));
%=============================================================================
r = nelson.ai.mcp.call('get_nelson_workspace');
assert_istrue(r.success);
assert_istrue(~isempty(r.data));
%=============================================================================
assert_checkerror('nelson.ai.mcp.call(''a_tool_that_does_not_exist'')', ...
  'Unknown tool: a_tool_that_does_not_exist.', 'Nelson:ai:unknownTool');
%=============================================================================
assert_checkerror('nelson.ai.mcp.call(42)', ...
  'a tool name expected.', 'Nelson:ai:toolNameExpected');
%=============================================================================
