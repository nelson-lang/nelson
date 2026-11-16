%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Agentic assistant loop: the model may request Nelson tools, which are run in
% the live session with nelson.ai.mcp.call, and the results are fed back until a
% final answer. Requires a provider reachable through nelson.ai.options/ask.
function varargout = agent(prompt, options)
  narginchk(1, 2);
  nargoutchk(0, 2);
  if ~ischar(prompt) && ~(isstring(prompt) && isscalar(prompt))
    error('Nelson:ai:promptMustBeText', 'prompt must be a character vector or string scalar.');
  end
  prompt = char(prompt);
  if nargin < 2
    options = nelson.ai.options();
  end
  % The claude-agent-acp backend is a full agent of its own: hand the prompt to
  % its persistent session instead of running the local tool loop.
  if isAcpBackend(options)
    varargout{1} = aiAgentAcp(prompt, options);
    if nargout > 1
      varargout{2} = '';
    end
    return
  end
  maxSteps = 8;
  if isfield(options, 'MaxSteps') && ~isempty(options.MaxSteps)
    maxSteps = double(options.MaxSteps);
  end
  systemPrompt = aiAgentSystemPrompt();
  if isfield(options, 'SystemPrompt') && ~isempty(options.SystemPrompt)
    systemPrompt = [systemPrompt, char(10), char(options.SystemPrompt)];
  end
  options.SystemPrompt = systemPrompt;
  transcript = ['User request: ', prompt];
  reply = '';
  lastObservation = '';
  for step = 1:maxSteps
    reply = nelson.ai.ask(transcript, options);
    options.Images = {};
    [isTool, name, args] = nelson.ai.parseToolCall(reply);
    if isTool
      [transcript, lastObservation, options] = runToolStep(transcript, reply, name, args, options);
      continue
    end
    % A reply that tried to be a tool call but could not be parsed (common with
    % small models emitting invalid JSON) is corrected rather than shown raw.
    if looksLikeToolAttempt(reply) && step < maxSteps
      transcript = appendCorrection(transcript, reply);
      continue
    end
    break
  end
  varargout{1} = finalReply(reply, lastObservation);
  if nargout > 1
    varargout{2} = transcript;
  end
end
%=============================================================================
function [transcript, observation, options] = runToolStep(transcript, reply, name, args, options)
  aiAgentEmit('activity', struct('kind', 'tool', 'name', name));
  result = nelson.ai.mcp.call(name, args);
  observation = aiAgentFormatObservation(name, result);
  aiAgentEmit('activity', struct('kind', 'result', 'name', name, ...
    'text', firstLine(observation)));
  transcript = [transcript, char(10), 'Assistant: ', reply, char(10), observation];
  if isfield(result, 'image') && ~isempty(result.image)
    options = nelson.ai.attachImage(options, result.image);
  end
end
%=============================================================================
function line = firstLine(text)
  text = char(text);
  nl = find(text == char(10), 1, 'first');
  if isempty(nl)
    line = text;
  else
    line = text(1:nl - 1);
  end
  if length(line) > 200
    line = [line(1:200), '…'];
  end
end
%=============================================================================
function transcript = appendCorrection(transcript, reply)
  correction = ['Observation: that was not a valid tool call. Reply with ONLY a JSON ', ...
    'object {"tool": "<name>", "arguments": {...}} with every newline inside a string ', ...
    'written as \n, or give the final answer as plain text.'];
  transcript = [transcript, char(10), 'Assistant: ', reply, char(10), correction];
end
%=============================================================================
% Never surface a raw tool-call payload to the user: if the loop ended while the
% model was still emitting one, answer with the last tool result instead.
function reply = finalReply(reply, lastObservation)
  if ~nelson.ai.parseToolCall(reply) && ~looksLikeToolAttempt(reply)
    return
  end
  if ~isempty(lastObservation)
    reply = lastObservation;
  else
    reply = 'The assistant could not complete the request within the step limit.';
  end
end
%=============================================================================
% True when the options select the claude-agent-acp backend rather than a
% direct provider (ollama, openai-compatible, ...).
function tf = isAcpBackend(options)
  tf = false;
  if isfield(options, 'Provider') && (ischar(options.Provider) || isstring(options.Provider)) ...
      && strcmpi(char(options.Provider), 'acp')
    tf = true;
    return
  end
  if isfield(options, 'Model') && (ischar(options.Model) || isstring(options.Model)) ...
      && strcmpi(char(options.Model), 'claude-agent-acp')
    tf = true;
  end
end
%=============================================================================
% True when a reply is clearly an attempted (but unparsed) tool call, so it must
% not be shown to the user as a final answer.
function tf = looksLikeToolAttempt(reply)
  if ~ischar(reply) && ~(isstring(reply) && isscalar(reply))
    tf = false;
    return
  end
  reply = char(reply);
  tf = ~isempty(regexp(reply, '"tool"\s*:\s*"', 'once')) ...
    && ~isempty(regexp(reply, '"arguments"\s*:', 'once'));
end
%=============================================================================
