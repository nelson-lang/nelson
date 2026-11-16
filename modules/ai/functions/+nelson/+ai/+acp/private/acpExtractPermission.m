%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Pull what a modal needs out of a session/request_permission request: the
% request id, the allow/reject option ids (so we can answer with the agent's own
% options), and the tool/path/title to show the user. Everything is best-effort
% and defensive: the shape of toolCall varies, so missing fields fall back to a
% generic prompt rather than failing.
function perm = acpExtractPermission(message)
  perm = struct('reqId', [], 'allowOptionId', '', 'rejectOptionId', '', ...
    'tool', '', 'kind', '', 'path', '', 'title', 'The assistant requests permission.');
  if ~isstruct(message) || ~isfield(message, 'id')
    return
  end
  perm.reqId = message.id;
  if ~isfield(message, 'params') || ~isstruct(message.params)
    return
  end
  params = message.params;
  perm = fillOptionIds(perm, params);
  perm = fillToolAndPath(perm, params);
  if ~isempty(perm.path)
    perm.title = ['The assistant requests access to ', perm.path, '.'];
  elseif ~isempty(perm.tool)
    perm.title = ['The assistant requests permission for ', perm.tool, '.'];
  end
end
%=============================================================================
function perm = fillOptionIds(perm, params)
  if ~isfield(params, 'options')
    return
  end
  opts = params.options;
  for k = 1:numel(opts)
    if iscell(opts)
      o = opts{k};
    else
      o = opts(k);
    end
    if ~isstruct(o) || ~isfield(o, 'optionId') || ~isfield(o, 'kind')
      continue
    end
    kind = lower(char(o.kind));
    if contains(kind, 'allow') && isempty(perm.allowOptionId)
      perm.allowOptionId = char(o.optionId);
    elseif contains(kind, 'reject') && isempty(perm.rejectOptionId)
      perm.rejectOptionId = char(o.optionId);
    end
  end
end
%=============================================================================
function perm = fillToolAndPath(perm, params)
  if ~isfield(params, 'toolCall') || ~isstruct(params.toolCall)
    return
  end
  tc = params.toolCall;
  if isfield(tc, 'kind')
    perm.kind = char(tc.kind);
  end
  if isfield(tc, 'title')
    perm.tool = char(tc.title);
  elseif ~isempty(perm.kind)
    perm.tool = perm.kind;
  end
  candidates = {};
  if isfield(tc, 'rawInput') && isstruct(tc.rawInput) && isfield(tc.rawInput, 'path')
    candidates{end + 1} = tc.rawInput.path;
  end
  if isfield(tc, 'locations')
    locs = tc.locations;
    if ~isempty(locs)
      if iscell(locs)
        first = locs{1};
      else
        first = locs(1);
      end
      if isstruct(first) && isfield(first, 'path')
        candidates{end + 1} = first.path;
      end
    end
  end
  for k = 1:numel(candidates)
    if ~isempty(candidates{k})
      perm.path = char(candidates{k});
      return
    end
  end
end
%=============================================================================
