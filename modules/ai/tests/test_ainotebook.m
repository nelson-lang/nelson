%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% insert_into_nelson_notebook is a pane-side tool: it pushes a code cell to the
% live notebook through __ai_notify__ and is a no-op when no host is attached.
r = nelson.ai.mcp.call('insert_into_nelson_notebook', struct('code', 'y = 2 + 2'));
assert_isequal(class(r), 'struct');
assert_istrue(isfield(r, 'success'));
assert_istrue(isfield(r, 'output'));
assert_istrue(isfield(r, 'error'));
assert_istrue(isfield(r, 'data'));
assert_istrue(isfield(r, 'duration'));
assert_istrue(r.success);
assert_istrue(contains(r.output, 'cell'));
%=============================================================================
% The run flag is reported in the tool result.
r = nelson.ai.mcp.call('insert_into_nelson_notebook', struct('code', 'y = 2 + 2', 'run', true));
assert_istrue(r.success);
assert_istrue(contains(r.output, 'ran'));
%=============================================================================
% Missing code is rejected with a clear message, like the editor tool.
r = nelson.ai.mcp.call('insert_into_nelson_notebook', struct('code', ''));
assert_isfalse(r.success);
assert_isequal(r.error, 'code is required.');
%=============================================================================
% The editor tool still routes the same way (regression on the shared handler).
r = nelson.ai.mcp.call('insert_into_nelson_editor', struct('code', 'y = 2 + 2'));
assert_istrue(r.success);
%=============================================================================
