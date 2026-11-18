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
% open_nelson_doc needs a topic (opening the real doc browser needs a GUI, so
% only the validation path is exercised here).
r = nelson.ai.mcp.call('open_nelson_doc', struct());
asserts.istrue(~r.success);
asserts.istrue(contains(r.error, 'topic'));
%=============================================================================
% open_nelson_file still requires the file to exist even with a line number; the
% line does not bypass the on-disk check.
missingLine = [tempdir(), 'nelson_no_such_assistant_file_line.m'];
if isfile(missingLine)
  delete(missingLine);
end
r = nelson.ai.mcp.call('open_nelson_file', struct('filename', missingLine, 'line', 10));
asserts.istrue(~r.success);
asserts.istrue(contains(r.error, 'does not exist'));
%=============================================================================
% run_current_file reports a clear failure when no saved file is open in the
% editor and no filename is given (the common headless case).
r = nelson.ai.mcp.call('run_current_file', struct());
asserts.istrue(~r.success);
asserts.istrue(contains(lower(r.error), 'file'));
% run_current_file with a filename that is not on disk fails gracefully (an unsaved
% draft or a wrong path), instead of running stale content.
missingRun = strrep([tempdir(), 'nelson_no_such_run_file_zzz.m'], '\', '/');
if isfile(missingRun)
  delete(missingRun);
end
r = nelson.ai.mcp.call('run_current_file', struct('filename', missingRun));
asserts.istrue(~r.success);
asserts.istrue(contains(r.error, 'does not exist'));
% run_current_file with a SAVED file runs it in the console WITHOUT needing the
% editor open (the gap the user hit: "The text editor is not open"). The script
% writes a sentinel file, so the execution is verifiable independently of scope.
runSentinel = strrep([tempdir(), 'nelson_run_console_sentinel.txt'], '\', '/');
if isfile(runSentinel)
  delete(runSentinel);
end
savedRun = strrep([tempdir(), 'nelson_run_console_test.m'], '\', '/');
fid = fopen(savedRun, 'w');
fprintf(fid, 'fid_sentinel = fopen(''%s'', ''w''); fclose(fid_sentinel);\n', runSentinel);
fclose(fid);
r = nelson.ai.mcp.call('run_current_file', struct('filename', savedRun));
asserts.istrue(r.success);
asserts.istrue(contains(r.output, 'console'));
asserts.istrue(contains(r.output, savedRun));
% the file actually ran: its side effect (the sentinel) is on disk
asserts.istrue(isfile(runSentinel));
if isfile(savedRun)
  delete(savedRun);
end
if isfile(runSentinel)
  delete(runSentinel);
end
%=============================================================================
% profile_current_file / debug_nelson_file / clear_breakpoints drive the profiler
% and debugger. They run a SAVED file (same bound as run_current_file) and work on
% both the Qt and the web surface (dbstop/profile/edit are surface-agnostic).
% A missing file is reported, not run.
missingDbg = strrep([tempdir(), 'nelson_no_such_debug_file_zzz.m'], '\', '/');
if isfile(missingDbg)
  delete(missingDbg);
end
r = nelson.ai.mcp.call('profile_current_file', struct('filename', missingDbg));
asserts.istrue(~r.success);
asserts.istrue(contains(r.error, 'does not exist'));
r = nelson.ai.mcp.call('debug_nelson_file', struct('filename', missingDbg, 'line', 2));
asserts.istrue(~r.success);
asserts.istrue(contains(r.error, 'does not exist'));
% With a saved file, profile_current_file runs it under the profiler and reports the
% top functions plus where the full report lives.
dbgFile = strrep([tempdir(), 'nelson_profile_debug_test.m'], '\', '/');
fid = fopen(dbgFile, 'w');
fprintf(fid, 'a = 0;\nfor k = 1:50\n  a = a + sum((1:100) .^ 2);\nend\n');
fclose(fid);
r = nelson.ai.mcp.call('profile_current_file', struct('filename', dbgFile));
asserts.istrue(r.success);
asserts.istrue(contains(r.output, 'Profiled'));
asserts.istrue(contains(r.output, 'Profiler panel'));
% the summary shows the user's file, not the host action's own call.m frames
asserts.istrue(~contains(r.output, 'call.m'));
% debug_nelson_file sets a breakpoint at the line (verifiable with dbstatus) and
% reports it; the editor open is a no-op without a pane.
dbclear('all');
r = nelson.ai.mcp.call('debug_nelson_file', struct('filename', dbgFile, 'line', 3));
asserts.istrue(r.success);
asserts.istrue(contains(r.output, 'breakpoint'));
asserts.istrue(~isempty(dbstatus()));
% clear_breakpoints with a filename clears that file's breakpoints.
r = nelson.ai.mcp.call('clear_breakpoints', struct('filename', dbgFile));
asserts.istrue(r.success);
asserts.istrue(isempty(dbstatus()));
% clear_breakpoints with no filename clears all of them.
dbstop('in', dbgFile, 'at', '2');
r = nelson.ai.mcp.call('clear_breakpoints', struct());
asserts.istrue(r.success);
asserts.istrue(isempty(dbstatus()));
if isfile(dbgFile)
  delete(dbgFile);
end
%=============================================================================
% propose_nelson_edit needs both a filename and the new content.
r = nelson.ai.mcp.call('propose_nelson_edit', struct());
asserts.istrue(~r.success);
asserts.istrue(contains(r.error, 'filename'));
r = nelson.ai.mcp.call('propose_nelson_edit', struct('filename', 'd:/x.m'));
asserts.istrue(~r.success);
asserts.istrue(contains(r.error, 'code'));
%=============================================================================
% propose_nelson_edit on an existing file computes a unified diff (shipped
% diff_file engine) of the proposed content against the on-disk version, and
% reports it (the editorInsert push is a no-op with no pane).
existing = [tempdir(), 'nelson_propose_edit_test.m'];
fid = fopen(existing, 'w');
fprintf(fid, 'a = 1;\n');
fclose(fid);
r = nelson.ai.mcp.call('propose_nelson_edit', struct('filename', existing, 'code', sprintf('a = 2;\n')));
asserts.istrue(r.success);
asserts.istrue(contains(r.output, 'Proposed changes'));
if isfile(existing)
  delete(existing);
end
%=============================================================================
% The claude-agent-acp bridge (acpRunHostActions) parses the <<nelson-action>>
% markers in-session and strips them from the reply shown to the user. It is a
% private helper, reached by putting its folder on the path for this test.
oldPath = path();
restorePath = onCleanup(@() path(oldPath));
addpath([modulepath('ai', 'functions'), '/+nelson/+ai/+internal/private']);
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
% It also tells the agent the marker must be valid JSON on one line (escape line
% breaks), the guidance that prevents the malformed-marker failure below.
asserts.istrue(contains(pre, 'valid JSON on a single line'));
% run_current_file is documented with its optional filename, so the agent can run a
% saved file in the console even when no editor is open.
asserts.istrue(contains(pre, 'run_current_file {"filename"'));
% the profiler/debugger actions are documented so the agent can drive those GUIs.
asserts.istrue(contains(pre, 'profile_current_file'));
asserts.istrue(contains(pre, 'debug_nelson_file'));
asserts.istrue(contains(pre, 'clear_breakpoints'));
%=============================================================================
% End to end through the bridge: a run_current_file marker carrying a filename runs
% that saved file in the console (no editor needed), and the marker is stripped. The
% script writes a sentinel so the execution is verifiable.
bridgeSentinel = strrep([tempdir(), 'nelson_run_bridge_sentinel.txt'], '\', '/');
if isfile(bridgeSentinel)
  delete(bridgeSentinel);
end
bridgeScript = strrep([tempdir(), 'nelson_run_bridge_test.m'], '\', '/');
fid = fopen(bridgeScript, 'w');
fprintf(fid, 'fid_b = fopen(''%s'', ''w''); fclose(fid_b);\n', bridgeSentinel);
fclose(fid);
marked = ['Running it. <<nelson-action>>{"tool": "run_current_file", "arguments": ', ...
  '{"filename": "', bridgeScript, '"}}<<end>>'];
cleaned = acpRunHostActions(marked, 'test');
asserts.istrue(~contains(cleaned, 'nelson-action'));
asserts.istrue(contains(cleaned, 'Running it.'));
asserts.istrue(isfile(bridgeSentinel));
if isfile(bridgeScript)
  delete(bridgeScript);
end
if isfile(bridgeSentinel)
  delete(bridgeSentinel);
end
%=============================================================================
% Regression (user report): "crée d:\demo.m, ouvre-le et mets-y le code". The agent
% emits insert_into_nelson_file with a MULTI-LINE code value and a Windows path, so
% the inline JSON has raw line breaks and a lone backslash and is invalid. It is
% now repaired before decoding, so the action runs (a no-op without a pane) and the
% reply confirms the editor instead of silently dropping it behind false prose.
code = sprintf('x = 1\ny = 2\nsurf(x)');
marked = ['<<nelson-action>>{"tool": "insert_into_nelson_file", "arguments": ', ...
  '{"filename": "d:\demo.m", "code": "', code, '"}}<<end>>'];
cleaned = acpRunHostActions(marked, 'test');
asserts.istrue(~contains(cleaned, 'nelson-action'));
asserts.istrue(contains(cleaned, 'editor'));
asserts.istrue(~contains(lower(cleaned), 'malformed'));
%=============================================================================
% A marker that names a real action but is structurally broken JSON cannot be
% repaired: it is surfaced as a plain note (not dropped in silence) and the model's
% own prose is kept, so the user is never left with a false success.
marked = ['It is open. <<nelson-action>>{"tool" "open_nelson_file" ', ...
  '"arguments": {}}<<end>>'];
cleaned = acpRunHostActions(marked, 'test');
asserts.istrue(~contains(cleaned, 'nelson-action'));
asserts.istrue(contains(lower(cleaned), 'malformed'));
asserts.istrue(contains(cleaned, 'It is open.'));
%=============================================================================
% An action that parses and RUNS but the host reports as failed (here opening a
% file that does not exist) is corrected in the reply: the failure reason is
% appended as a note so the model's optimistic prose cannot stand as a false
% success, while the model's own text is kept.
missing2 = strrep([tempdir(), 'nelson_no_such_assistant_file_yyy.m'], '\', '/');
if isfile(missing2)
  delete(missing2);
end
marked = ['It is open now. <<nelson-action>>{"tool": "open_nelson_file", ', ...
  '"arguments": {"filename": "', missing2, '"}}<<end>>'];
[cleaned, failureText] = acpRunHostActions(marked, 'test');
asserts.istrue(~contains(cleaned, 'nelson-action'));
asserts.istrue(contains(cleaned, 'It is open now.'));
asserts.istrue(contains(lower(cleaned), 'does not exist'));
% The second output carries the failure reason, which acpAsyncStep feeds back to
% the agent for a bounded self-correction.
asserts.istrue(contains(lower(failureText), 'does not exist'));
%=============================================================================
% A reply whose action succeeds reports no failure text (nothing to feed back).
[~, okFailure] = acpRunHostActions('Done.', 'test');
asserts.istrue(isempty(okFailure));
%=============================================================================
% A data-producing action (profiling) returns its result as the THIRD output, so
% acpAsyncStep can feed it back to the agent to analyze (instead of the agent only
% opening the panel and asking the user what it shows).
dataScript = strrep([tempdir(), 'nelson_acp_profile_data.m'], '\', '/');
fid = fopen(dataScript, 'w');
fprintf(fid, 'a = 0;\nfor k = 1:30\n  a = a + sum((1:50) .^ 2);\nend\n');
fclose(fid);
marked = ['Profiling it. <<nelson-action>>{"tool": "profile_current_file", "arguments": ', ...
  '{"filename": "', dataScript, '"}}<<end>>'];
[cleaned, failTxt, dataTxt] = acpRunHostActions(marked, 'test');
asserts.istrue(~contains(cleaned, 'nelson-action'));
asserts.istrue(isempty(failTxt));
asserts.istrue(~isempty(dataTxt));
asserts.istrue(contains(dataTxt, 'Profiled'));
if isfile(dataScript)
  delete(dataScript);
end
% A non-data action (and a plain reply) produces no data feedback.
[~, ~, dataNone] = acpRunHostActions('Done.', 'test');
asserts.istrue(isempty(dataNone));
%=============================================================================
