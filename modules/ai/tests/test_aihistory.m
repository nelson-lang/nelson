%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Persistent per-surface conversation history: append / load / recent / clear.
% Uses a dedicated surface id so it never touches a real conversation, and clears
% it on cleanup.
sid = 'test_history_surface_zzz';
cleaner = onCleanup(@() nelson.ai.internal.history.clear(sid)); %#ok<NASGU>
nelson.ai.internal.history.clear(sid);
%=============================================================================
% Empty history: load is an empty struct array, recent is '', json is '[]'.
t = nelson.ai.internal.history.load(sid);
assert_istrue(isempty(t));
assert_isequal(nelson.ai.internal.history.recent(sid), '');
assert_isequal(nelson.ai.internal.history.json(sid), '[]');
%=============================================================================
% Append a user turn then an assistant turn; they round-trip through the file.
nelson.ai.internal.history.append(sid, 'user', 'first question');
nelson.ai.internal.history.append(sid, 'assistant', 'first answer');
t = nelson.ai.internal.history.load(sid);
assert_isequal(numel(t), 2);
assert_isequal(t(1).role, 'user');
assert_isequal(t(1).text, 'first question');
assert_isequal(t(2).role, 'assistant');
assert_isequal(t(2).text, 'first answer');
%=============================================================================
% recent() formats the turns for prompt injection.
r = nelson.ai.internal.history.recent(sid, 6);
assert_istrue(contains(r, 'User: first question'));
assert_istrue(contains(r, 'Assistant: first answer'));
%=============================================================================
% json() is always a JSON array (even for one turn) a host pane can parse to
% re-display the conversation on open.
decoded = jsondecode(nelson.ai.internal.history.json(sid));
assert_isequal(numel(decoded), 2);
assert_isequal(decoded(1).role, 'user');
assert_isequal(decoded(1).text, 'first question');
assert_isequal(decoded(2).role, 'assistant');
assert_isequal(decoded(2).text, 'first answer');
%=============================================================================
% An unknown role or an empty text is ignored (still 2 turns).
nelson.ai.internal.history.append(sid, 'system', 'ignored');
nelson.ai.internal.history.append(sid, 'user', '');
assert_isequal(numel(nelson.ai.internal.history.load(sid)), 2);
%=============================================================================
% The stored history is capped at 100 turns (oldest dropped).
nelson.ai.internal.history.clear(sid);
for k = 1:120
  nelson.ai.internal.history.append(sid, 'user', ['q', int2str(k)]);
end
t = nelson.ai.internal.history.load(sid);
assert_isequal(numel(t), 100);
assert_isequal(t(1).text, 'q21');
assert_isequal(t(end).text, 'q120');
%=============================================================================
% recent(n) returns only the last n turns.
r = nelson.ai.internal.history.recent(sid, 3);
assert_istrue(contains(r, 'q118'));
assert_istrue(contains(r, 'q120'));
assert_isfalse(contains(r, 'q117'));
%=============================================================================
% clear() forgets the conversation.
nelson.ai.internal.history.clear(sid);
assert_istrue(isempty(nelson.ai.internal.history.load(sid)));
%=============================================================================
% The Qt dock and the web pane SHARE one conversation (mutualized): a turn stored
% under 'qt' is visible under 'web' and vice versa, while another surface stays
% separate. The shared file is backed up and restored on cleanup so a real
% conversation is never disturbed.
mainFile = [prefdir(), '/assistant/history/conversation_main.json'];
mainBak = [tempdir(), 'nelson_main_hist_test_bak.json'];
hadMain = isfile(mainFile);
if hadMain
  copyfile(mainFile, mainBak);
end
restoreMain = onCleanup(@() restoreMainHistory(mainFile, mainBak, hadMain)); %#ok<NASGU>
nelson.ai.internal.history.clear('qt');
assert_istrue(isempty(nelson.ai.internal.history.load('qt')));
assert_istrue(isempty(nelson.ai.internal.history.load('web')));
nelson.ai.internal.history.append('qt', 'user', 'from_qt');
nelson.ai.internal.history.append('web', 'assistant', 'from_web');
% both surfaces see both turns (one shared conversation)
shared = nelson.ai.internal.history.load('web');
assert_isequal(numel(shared), 2);
assert_isequal(shared(1).text, 'from_qt');
assert_isequal(shared(2).text, 'from_web');
assert_isequal(numel(nelson.ai.internal.history.load('qt')), 2);
% a different surface does NOT share the 'main' conversation
assert_istrue(isempty(nelson.ai.internal.history.load(sid)));
% clearing from either surface clears the shared conversation
nelson.ai.internal.history.clear('web');
assert_istrue(isempty(nelson.ai.internal.history.load('qt')));
%=============================================================================
% newConversation() forgets a surface's persisted conversation (and resets its live
% ACP agent, a no-op here with none). A non-shared surface id keeps this off the
% real 'main' conversation.
nc = 'test_newconv_surface_zzz';
nelson.ai.internal.history.clear(nc);
nelson.ai.internal.history.append(nc, 'user', 'to be forgotten');
assert_isequal(numel(nelson.ai.internal.history.load(nc)), 1);
nelson.ai.internal.newConversation(nc);
assert_istrue(isempty(nelson.ai.internal.history.load(nc)));
%=============================================================================
function restoreMainHistory(mainFile, mainBak, hadMain)
  try
    if isfile(mainFile)
      delete(mainFile);
    end
    if hadMain && isfile(mainBak)
      copyfile(mainBak, mainFile);
      delete(mainBak);
    end
  catch
  end
end
%=============================================================================
