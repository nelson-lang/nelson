%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Answer a JSON-RPC REQUEST the agent sent to us (a message carrying both a
% 'method' and its own 'id'). An unanswered request blocks the agent forever, so a
% prompt that makes the agent ask for a tool (for example editing a file) would
% otherwise hang on "Thinking..." indefinitely. We decline: a permission prompt is
% rejected (the assistant drives the live session through host-action markers, not
% the agent's own sandbox tools), and any other client request is refused. The run
% then completes with the agent's text instead of stalling.
function acpAnswerAgentRequest(proc, message)
  narginchk(2, 2);
  if ~isstruct(message) || ~isfield(message, 'id') || ~isfield(message, 'method')
    return
  end
  reqId = message.id;
  if strcmp(char(message.method), 'session/request_permission')
    optionId = rejectOptionId(message);
    if isempty(optionId)
      outcome = struct('outcome', 'cancelled');
    else
      outcome = struct('outcome', 'selected', 'optionId', optionId);
    end
    response = struct('jsonrpc', '2.0', 'id', reqId, ...
      'result', struct('outcome', outcome));
  else
    response = struct('jsonrpc', '2.0', 'id', reqId, ...
      'error', struct('code', -32601, 'message', 'Client does not provide this capability.'));
  end
  try
    proc.write([jsonencode(response), char(10)]);
  catch
  end
end
%=============================================================================
% Pick the id of a reject-kind permission option so the agent abandons the tool
% but keeps answering. Returns '' when none is offered (the caller then cancels).
function optionId = rejectOptionId(message)
  optionId = '';
  if ~isfield(message, 'params') || ~isfield(message.params, 'options')
    return
  end
  options = message.params.options;
  for k = 1:numel(options)
    if iscell(options)
      opt = options{k};
    else
      opt = options(k);
    end
    if isstruct(opt) && isfield(opt, 'kind') && isfield(opt, 'optionId') ...
        && contains(lower(char(opt.kind)), 'reject')
      optionId = char(opt.optionId);
      return
    end
  end
end
%=============================================================================
