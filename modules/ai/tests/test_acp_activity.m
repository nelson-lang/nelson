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
% <--NOT FIX FOR WASM-->
%=============================================================================
% The activity feed turns an ACP tool-call update into one line so the pane shows
% what the agent does (read/write a file, run a tool), not just "thinking". Only
% the initial tool_call announces an action; text chunks and status updates do not.
addpath([modulepath('ai', 'functions'), '/+nelson/+ai/private']);
%=============================================================================
% The agent's own title is used verbatim when present.
act = acpUpdateActivity(struct('sessionUpdate', 'tool_call', 'title', 'Read d:/demo.m'));
asserts.isequal(act.kind, 'call');
asserts.isequal(act.name, 'Read d:/demo.m');
%=============================================================================
% Without a title, the label is the tool kind plus the rawInput path.
act = acpUpdateActivity(struct('sessionUpdate', 'tool_call', 'kind', 'read', ...
  'rawInput', struct('path', 'd:/x.m')));
asserts.isequal(act.name, 'read d:/x.m');
%=============================================================================
% The path can instead come from the reported locations.
act = acpUpdateActivity(struct('sessionUpdate', 'tool_call', 'kind', 'edit', ...
  'locations', struct('path', 'd:/y.m')));
asserts.isequal(act.name, 'edit d:/y.m');
%=============================================================================
% A plain text chunk is not an activity (it streams as a token).
asserts.istrue(isempty(acpUpdateActivity(struct('sessionUpdate', 'agent_message_chunk', ...
  'content', struct('type', 'text', 'text', 'hello')))));
%=============================================================================
% A status-only update does not repeat the announcement; a malformed update is safe.
asserts.istrue(isempty(acpUpdateActivity(struct('sessionUpdate', 'tool_call_update', ...
  'status', 'in_progress'))));
asserts.istrue(isempty(acpUpdateActivity(struct('foo', 'bar'))));
asserts.istrue(isempty(acpUpdateActivity([])));
%=============================================================================
