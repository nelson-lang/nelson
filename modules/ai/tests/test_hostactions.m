%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% The host-action tools (open_nelson_file / launch_nflow / open_nelson_notebook)
% are dispatched pane-side in nelson.ai.mcp.call so the assistant can pilot the
% widgets. They run on the live interpreter; here we check the routing and the
% argument validation (opening a real widget needs a GUI, so it is not asserted).
%=============================================================================
% open_nelson_file without a filename is rejected by its own handler (proving the
% dispatch reached it rather than the native tool registry).
r = nelson.ai.mcp.call('open_nelson_file', struct());
asserts.istrue(isstruct(r));
asserts.istrue(~r.success);
asserts.istrue(contains(r.error, 'filename'));
% open_nelson_file on a missing file fails gracefully instead of reaching edit(),
% which would pop a native "create it?" dialog (e.g. a draft not yet saved).
missing = [tempdir(), 'nelson_no_such_assistant_file_zzz.m'];
if isfile(missing)
  delete(missing);
end
r = nelson.ai.mcp.call('open_nelson_file', struct('filename', missing));
asserts.istrue(~r.success);
asserts.istrue(contains(r.error, 'does not exist'));
%=============================================================================
% open_nelson_notebook without a filename is rejected the same way.
r = nelson.ai.mcp.call('open_nelson_notebook', struct());
asserts.istrue(isstruct(r));
asserts.istrue(~r.success);
asserts.istrue(contains(r.error, 'filename'));
%=============================================================================
% insert_into_nelson_file needs both a filename and code (it opens the file and
% drops the code into the editor for the user to review/save).
r = nelson.ai.mcp.call('insert_into_nelson_file', struct());
asserts.istrue(~r.success);
asserts.istrue(contains(r.error, 'filename'));
r = nelson.ai.mcp.call('insert_into_nelson_file', struct('filename', 'd:/x.m'));
asserts.istrue(~r.success);
asserts.istrue(contains(r.error, 'code'));
% With both, it reports success (the editorInsert push is a no-op with no pane).
r = nelson.ai.mcp.call('insert_into_nelson_file', struct('filename', 'd:/x.m', 'code', 'y=1;'));
asserts.istrue(r.success);
asserts.istrue(contains(r.output, 'd:/x.m'));
%=============================================================================
% Every host action returns the common tool-result shape. launch_nflow is wired
% the same way in the switch, but calling it would open a real widget window, so
% only the validated no-op paths are exercised here.
r = nelson.ai.mcp.call('open_nelson_file', struct());
asserts.istrue(isfield(r, 'success'));
asserts.istrue(isfield(r, 'output'));
asserts.istrue(isfield(r, 'error'));
asserts.istrue(isfield(r, 'duration'));
%=============================================================================
% The claude-agent-acp bridge (acpRunHostActions) parses the <<nelson-action>>
% markers in-session and strips them from the reply shown to the user. It is a
% private helper, reached by putting its folder on the path for this test.
oldPath = path();
restorePath = onCleanup(@() path(oldPath));
addpath([modulepath('ai', 'functions'), '/+nelson/+ai/private']);
%=============================================================================
marked = sprintf('Done.\n<<nelson-action>>{"tool": "open_nelson_file", "arguments": {}}<<end>>');
cleaned = acpRunHostActions(marked, 'test');
asserts.istrue(~contains(cleaned, 'nelson-action'));
asserts.istrue(contains(cleaned, 'Done.'));
%=============================================================================
% A reply with no marker is returned unchanged.
asserts.isequal(acpRunHostActions('plain reply', 'test'), 'plain reply');
%=============================================================================
% A marker naming a tool OUTSIDE the host-action allowlist (here arbitrary code
% execution) is ignored: it never runs, and the marker is still stripped. The
% sentinel file would only exist if the disallowed tool had been dispatched.
sentinel = strrep([tempdir(), 'nelson_acp_allowlist_sentinel.txt'], '\', '/');
if isfile(sentinel)
  delete(sentinel);
end
code = sprintf('fid=fopen(''%s'',''w'');fclose(fid);', sentinel);
marked = ['a <<nelson-action>>{"tool": "evaluate_nelson_code", "arguments": {"code": "', ...
  code, '"}}<<end>> b'];
cleaned = acpRunHostActions(marked, 'test');
asserts.istrue(~isfile(sentinel));
asserts.istrue(~contains(cleaned, 'nelson-action'));
if isfile(sentinel)
  delete(sentinel);
end
%=============================================================================
% An action-only reply (markers, no prose) is replaced by a short honest
% confirmation, so the pane never renders an empty "(no response)" bubble.
% insert_into_nelson_file succeeds as a no-op without a pane.
marked = ['<<nelson-action>>{"tool": "insert_into_nelson_file", "arguments": ', ...
  '{"filename": "d:/x.m", "code": "y=1;"}}<<end>>'];
cleaned = acpRunHostActions(marked, 'test');
asserts.istrue(~isempty(cleaned));
asserts.istrue(~contains(cleaned, 'nelson-action'));
asserts.istrue(contains(cleaned, 'editor'));
% The confirmation must never claim the draft was saved or created.
asserts.istrue(~contains(lower(cleaned), 'saved'));
asserts.istrue(~contains(lower(cleaned), 'created'));
%=============================================================================
% A markers-only reply whose only action is disallowed runs nothing, so no
% confirmation is fabricated: the cleaned reply stays empty.
marked = ['<<nelson-action>>{"tool": "evaluate_nelson_code", "arguments": ', ...
  '{"code": "x=1;"}}<<end>>'];
cleaned = acpRunHostActions(marked, 'test');
asserts.istrue(isempty(cleaned));
%=============================================================================
% The host-action preamble tells the agent to always confirm in words and not to
% over-claim that an inserted draft was saved.
pre = acpHostActionPreamble();
asserts.istrue(contains(pre, 'never reply with only a marker'));
asserts.istrue(contains(pre, 'NOT saved'));
%=============================================================================
