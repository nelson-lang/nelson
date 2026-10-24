%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = aiask(prompt, options)
  narginchk(1, 2);
  nargoutchk(0, 2);
  prompt = getTextScalar(prompt, 'prompt');
  if nargin < 2
    options = aioptions();
  end
  if ~isstruct(options) || ~isscalar(options)
    error('Nelson:ai:optionsAScalarStructReturned', 'options must be a scalar struct returned by aioptions.');
  end
  provider = lower(options.Provider);
  switch provider
    case 'ollama'
      response = askOllama(prompt, options);
      text = response.response;
    case 'openai-compatible'
      response = askOpenAICompatible(prompt, options);
      text = extractOpenAICompatibleText(response);
    case 'codex-cli'
      [text, response] = askCodexCli(prompt, options);
    otherwise
      error('Nelson:ai:unsupportedAIProvider', 'Unsupported AI provider.');
  end
  varargout{1} = text;
  if nargout > 1
    varargout{2} = response;
  end
end
%=============================================================================
function response = askOllama(prompt, options)
  payload = struct();
  payload.model = options.Model;
  payload.prompt = prompt;
  payload.stream = false;
  if isfield(options, 'SystemPrompt') && ~isempty(options.SystemPrompt)
    payload.system = options.SystemPrompt;
  end
  if isfield(options, 'Think') && ~isempty(options.Think)
    payload.think = logical(options.Think);
  end
  modelOptions = struct();
  if isfield(options, 'NumPredict') && ~isempty(options.NumPredict)
    modelOptions.num_predict = options.NumPredict;
  end
  if isfield(options, 'Temperature') && ~isempty(options.Temperature)
    modelOptions.temperature = options.Temperature;
  end
  if ~isempty(fieldnames(modelOptions))
    payload.options = modelOptions;
  end
  webOpts = weboptions();
  webOpts.RequestMethod = 'POST';
  webOpts.ContentType = 'json';
  webOpts.MediaType = 'application/json';
  webOpts.Timeout = options.Timeout;
  response = webwrite(options.Endpoint, jsonencode(payload), webOpts);
end
%=============================================================================
function response = askOpenAICompatible(prompt, options)
  payload = struct();
  payload.model = options.Model;
  payload.messages = struct([]);
  index = 1;
  if isfield(options, 'SystemPrompt') && ~isempty(options.SystemPrompt)
    payload.messages(index).role = 'system';
    payload.messages(index).content = options.SystemPrompt;
    index = index + 1;
  end
  payload.messages(index).role = 'user';
  payload.messages(index).content = prompt;
  webOpts = weboptions();
  webOpts.RequestMethod = 'POST';
  webOpts.ContentType = 'json';
  webOpts.MediaType = 'application/json';
  webOpts.Timeout = options.Timeout;
  if isfield(options, 'TokenEnvVar') && ~isempty(options.TokenEnvVar)
    token = getenv(options.TokenEnvVar);
    if isempty(token)
      error('Nelson:ai:aIProviderTokenEnvironmentVariable', 'AI provider token environment variable is not defined.');
    end
    webOpts.HeaderFields = {'Authorization', ['Bearer ', token]; 'ContentType', 'application/json'};
  end
  response = webwrite(options.Endpoint, jsonencode(payload), webOpts);
end
%=============================================================================
function text = extractOpenAICompatibleText(response)
  text = '';
  if isfield(response, 'choices') && length(response.choices) >= 1
    choice = response.choices(1);
    if isfield(choice, 'message') && isfield(choice.message, 'content')
      text = choice.message.content;
      return
    end
    if isfield(choice, 'text')
      text = choice.text;
    end
  end
end
%=============================================================================
function [text, response] = askCodexCli(prompt, options)
  schema = [tempname(), '.json'];
  fid = fopen(schema, 'wt');
  fwrite(fid, '{"type":"object","properties":{"answer":{"type":"string"}},"required":["answer"],"additionalProperties":false}');
  fclose(fid);
  cleanup = onCleanup(@() rmfile(schema));
  command = ['codex exec --json --output-schema "', schema, '" "', escapeShellText(prompt), '"'];
  [status, output] = system(command, options.Timeout);
  response = struct();
  response.status = status;
  response.output = output;
  if status ~= 0
    error('Nelson:aiask:commandFailed', output);
  end
  text = output;
end
%=============================================================================
function txt = getTextScalar(value, name)
  if isstring(value)
    if ~isscalar(value)
      error('Nelson:validators:mustBeTextScalar', '%s must be a character vector or string scalar.', name);
    end
    txt = char(value);
    return
  end
  if ~ischar(value) || size(value, 1) ~= 1
    error('Nelson:validators:mustBeTextScalar', '%s must be a character vector or string scalar.', name);
  end
  txt = value;
end
%=============================================================================
function txt = escapeShellText(txt)
  txt = strrep(txt, '"', '\"');
end
%=============================================================================
