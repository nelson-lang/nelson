%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Instruction prepended to the claude-agent-acp prompt so the agent can pilot the
% LIVE Nelson GUI. The agent's own file tools run in a sandbox and cannot reach
% the running session, so widget actions are requested through a marker that the
% in-session poll (acpRunHostActions) parses and executes. This bridge does not
% use the MCP subprocess, so it works whether or not NELSON_AI_ACP_MCP is set.
function s = acpHostActionPreamble()
  lines = {};
  lines{end + 1} = 'You can drive the live Nelson GUI (open files, launch tools).';
  lines{end + 1} = 'Your own file tools cannot reach the running session, so to act on it';
  lines{end + 1} = 'emit a marker on its own line, exactly:';
  lines{end + 1} = '<<nelson-action>>{"tool": "<name>", "arguments": {<arguments>}}<<end>>';
  lines{end + 1} = 'Available actions:';
  lines{end + 1} = '- open_nelson_file {"filename": "..."}: open an existing file in the editor.';
  lines{end + 1} = '- launch_nflow {"model": "..."}: open the NFlow editor (model is optional).';
  lines{end + 1} = '- open_nelson_notebook {"filename": "..."}: open a notebook document.';
  lines{end + 1} = ['- insert_into_nelson_file {"filename": "...", "code": "..."}: open a file and ', ...
    'drop code into the editor for the user to review and save. Use this to add/insert code ', ...
    'into a file instead of saying you cannot edit it.'];
  lines{end + 1} = ['The marker must be valid JSON on a single line: inside it escape every ', ...
    'backslash as \\ (so a path d:\\demo.m), every double quote as \", and every line ', ...
    'break in a code value as \n. Do not wrap the marker in a code fence.'];
  lines{end + 1} = 'Emit the marker when the user asks to open/edit a file or launch a tool, then confirm in words.';
  lines{end + 1} = 'Always add a short sentence after the marker; never reply with only a marker.';
  lines{end + 1} = ['insert_into_nelson_file only drops a draft into the editor: it is NOT saved. ', ...
    'Never say a file was saved, created, or written; say the code is in the editor to review ', ...
    'and save, and do not claim a file exists until the user has saved it.'];
  s = strjoin(lines, char(10));
end
%=============================================================================
