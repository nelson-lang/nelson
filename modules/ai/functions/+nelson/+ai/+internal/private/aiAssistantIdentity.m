%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Always-on identity and ground rules for the assistant, prepended to every
% request regardless of keyword matches. The ACP agent gets no system prompt
% (session/new only carries cwd), so this preamble is where the agent learns it
% is Nelson's assistant and must not answer as a generic MATLAB-flavored model.
%
% The wording lives in resources/assistant/identity.md (easier to edit, and
% shipped in the binary by both the CMake install and module.iss); this function
% reads it and appends the surface-specific note. An inline fallback keeps the
% assistant working if the resource is ever missing.
% surfaceId adds a one-line note about where the session runs.
function text = aiAssistantIdentity(surfaceId)
  narginchk(0, 1);
  if nargin < 1
    surfaceId = '';
  end
  body = identityBody();
  note = surfaceNote(char(surfaceId));
  if isempty(note)
    text = body;
  else
    text = [body, char(10), note];
  end
end
%=============================================================================
function body = identityBody()
  body = '';
  try
    mdPath = [modulepath('ai', 'root'), '/resources/assistant/identity.md'];
    if isfile(mdPath)
      body = strtrim(fileread(mdPath));
    end
  catch
    body = '';
  end
  if isempty(body)
    body = fallbackIdentity();
  end
end
%=============================================================================
% Used only if the resource file cannot be read; keep it in sync with
% resources/assistant/identity.md.
function body = fallbackIdentity()
  lines = {};
  lines{end + 1} = ['You are the AI assistant built into Nelson, a free and ', ...
    'open-source numerical computing language and environment with a ', ...
    'MATLAB-compatible syntax.'];
  lines{end + 1} = ['Nelson is its own product. Never name or use MATLAB, ', ...
    'Simulink, or other MathWorks product or function identifiers in your ', ...
    'answers; use Nelson''s own function names and idioms.'];
  lines{end + 1} = ['Answer questions about USING Nelson: its language, ', ...
    'functions, graphics, and toolboxes. Prefer concise, runnable Nelson code ', ...
    '(comments start with %). Nelson arrays are 1-based.'];
  lines{end + 1} = ['When a "Verified Nelson API" block is provided below, treat ', ...
    'those signatures as authoritative. If you are unsure whether a function or ', ...
    'signature exists, say so instead of inventing one.'];
  body = strjoin(lines, [char(10), char(10)]);
end
%=============================================================================
function note = surfaceNote(surfaceId)
  switch surfaceId
    case 'nflow'
      note = ['This session runs inside the NFlow block-diagram editor; follow ', ...
        'the NFlow build guide below for how to answer.'];
    case {'qt', 'web'}
      note = 'This session runs in the Nelson desktop assistant panel.';
    otherwise
      note = '';
  end
end
%=============================================================================
