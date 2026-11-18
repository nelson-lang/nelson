%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Send a prompt to a connected ACP agent and return its text answer. An optional
% onNotification callback streams updates; optional images (a cell of base64 PNG
% strings) are sent as image content blocks for a vision-capable agent.
function [text, client] = prompt(client, message, onNotification, images)
  narginchk(2, 4);
  if nargin < 3
    onNotification = [];
  end
  if nargin < 4
    images = {};
  end
  if ~isfield(client, 'proc') || ~isvalid(client.proc)
    error('Nelson:ai:acpNotConnected', 'The ACP client is not connected.');
  end
  timeoutSeconds = 120;
  id = client.nextId;
  client.nextId = id + 1;
  % The prompt is an array of content blocks ({type:'text'|'image', ...}).
  params = struct('sessionId', client.sessionId);
  blocks = {struct('type', 'text', 'text', char(message))};
  for k = 1:numel(images)
    blocks{end + 1} = struct('type', 'image', 'mimeType', 'image/png', ...
      'data', char(images{k}));
  end
  params.prompt = blocks;
  [result, notifications] = acpSendRequest(client.proc, id, 'session/prompt', params, ...
    timeoutSeconds, onNotification);
  text = '';
  for k = 1:numel(notifications)
    note = notifications{k};
    if isfield(note, 'params') && isfield(note.params, 'update')
      text = [text, contentText(note.params.update)];
    end
  end
  if isempty(text) && isfield(result, 'content')
    text = contentText(result);
  end
end
%=============================================================================
% Extract the text of an ACP content holder, tolerating a plain-string content
% and the structured {type:'text', text:...} form.
function out = contentText(holder)
  out = '';
  if ~isstruct(holder) || ~isfield(holder, 'content')
    return
  end
  content = holder.content;
  if ischar(content) || (isstring(content) && isscalar(content))
    out = char(content);
  elseif isstruct(content) && isscalar(content) && isfield(content, 'text')
    out = char(content.text);
  end
end
%=============================================================================
