%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Compact, read-only snapshot of the LIVE session so the model answers in context
% (it can refer to variables the user already has, the last error, and the file
% open in the editor) without the user restating them. Values are NEVER included,
% only names/sizes/classes, and everything is capped, so the prompt stays small
% and no workspace data leaks. Each part is omitted when empty; returns '' when
% nothing is known. It is awareness only, not an instruction to change anything.
function s = aiLiveSessionContext(surfaceId)
  narginchk(1, 1);
  s = '';
  parts = {};
  parts = appendPart(parts, workspaceSummary());
  parts = appendPart(parts, nflowModelSummary());
  parts = appendPart(parts, lastErrorSummary());
  parts = appendPart(parts, openEditorFile());
  if isempty(parts)
    return
  end
  header = ['Live session state (for your awareness only; do not change these ', ...
    'unless the user asks):'];
  s = [header, char(10), strjoin(parts, char(10))];
end
%=============================================================================
function parts = appendPart(parts, text)
  if ~isempty(text)
    parts{end + 1} = text;
  end
end
%=============================================================================
function text = workspaceSummary()
  text = '';
  try
    w = evalin('base', 'whos');
  catch
    return
  end
  if isempty(w)
    return
  end
  maxVars = 40;
  n = min(numel(w), maxVars);
  items = cell(1, n);
  for k = 1:n
    items{k} = [w(k).name, ' (', sizeText(w(k).size), ' ', char(w(k).class), ')'];
  end
  suffix = '';
  if numel(w) > maxVars
    suffix = [', and ', int2str(numel(w) - maxVars), ' more'];
  end
  text = ['Workspace variables: ', strjoin(items, ', '), suffix, '.'];
end
%=============================================================================
function t = sizeText(sz)
  sz = double(sz);
  parts = cell(1, numel(sz));
  for k = 1:numel(sz)
    parts{k} = int2str(sz(k));
  end
  t = strjoin(parts, 'x');
end
%=============================================================================
% Compact snapshot of the NFlow model currently open in the session (the one the
% pane shows), so the assistant edits/simulates THAT model instead of asking the
% user to restate it: its name, whether it has unsaved changes, and its blocks with
% their types. Names/types only, capped; '' when no model is open or the NFlow
% engine is absent. Wiring is not listed (the lean model API does not expose it
% reliably); block presence and type is the awareness that matters here.
function text = nflowModelSummary()
  text = '';
  try
    model = char(gcs());
  catch
    return
  end
  if isempty(model)
    return
  end
  blocks = modelBlockList(model);
  dirtyNote = '';
  try
    if bdIsDirty(model)
      dirtyNote = ' (unsaved changes)';
    end
  catch
  end
  text = ['NFlow model open: ''', model, '''', dirtyNote, '.'];
  if ~isempty(blocks)
    text = [text, ' Blocks: ', strjoin(blocks, ', '), '.'];
  end
end
%=============================================================================
% Short "name (type)" entries for the blocks directly under a model, capped. The
% model root itself is skipped (it carries no BlockType).
function items = modelBlockList(model)
  items = {};
  try
    names = find_system(model, 'SearchDepth', 1, 'Type', 'block');
  catch
    return
  end
  names = reshape(cellstr(names), 1, []);
  maxBlocks = 40;
  for k = 1:numel(names)
    full = names{k};
    if strcmp(full, model)
      continue
    end
    short = full;
    slash = find(full == '/', 1, 'last');
    if ~isempty(slash)
      short = full(slash + 1:end);
    end
    blockType = '';
    try
      blockType = char(get_param(full, 'BlockType'));
    catch
    end
    if isempty(blockType)
      items{end + 1} = short; %#ok<AGROW>
    else
      items{end + 1} = [short, ' (', blockType, ')']; %#ok<AGROW>
    end
    if numel(items) >= maxBlocks
      items{end + 1} = '...'; %#ok<AGROW>
      break
    end
  end
end
%=============================================================================
function text = lastErrorSummary()
  text = '';
  try
    le = char(lasterr());
  catch
    return
  end
  if isempty(le)
    return
  end
  text = ['Last error in the session: ', firstLine(le)];
end
%=============================================================================
function text = openEditorFile()
  text = '';
  try
    cf = char(__editor__('currentFile'));
  catch
    cf = '';
  end
  if isempty(cf)
    return
  end
  text = ['File open in the editor: ', cf, '.'];
end
%=============================================================================
function line = firstLine(text)
  text = char(text);
  nl = find(text == char(10), 1, 'first');
  if ~isempty(nl)
    text = text(1:nl - 1);
  end
  maxLen = 200;
  if numel(text) > maxLen
    text = [text(1:maxLen), '...'];
  end
  line = strtrim(text);
end
%=============================================================================
