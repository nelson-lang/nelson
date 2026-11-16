%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Send a prompt to a connected ACP agent WITHOUT waiting for the reply: the
% response is then read incrementally with nelson.ai.acp.poll. Optional images
% (cell of base64 PNGs) are sent as image content blocks. Keeps the interpreter
% free so the console stays usable while the model works.
function client = send(client, message, images)
  narginchk(2, 3);
  nargoutchk(0, 1);
  if nargin < 3
    images = {};
  end
  if ~isfield(client, 'proc') || ~isvalid(client.proc)
    error('Nelson:ai:acpNotConnected', 'The ACP client is not connected.');
  end
  id = client.nextId;
  client.nextId = id + 1;
  params = struct('sessionId', client.sessionId);
  blocks = {struct('type', 'text', 'text', char(message))};
  for k = 1:numel(images)
    blocks{end + 1} = struct('type', 'image', 'mimeType', 'image/png', ...
      'data', char(images{k})); %#ok<AGROW>
  end
  params.prompt = blocks;
  request = struct('jsonrpc', '2.0', 'id', id, 'method', 'session/prompt', 'params', params);
  client.proc.write([jsonencode(request), char(10)]);
  client.pending = struct('id', id, 'text', '', 'active', true);
end
%=============================================================================
