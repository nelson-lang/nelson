%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = aimodels(options)
  narginchk(0, 1);
  nargoutchk(0, 2);
  if nargin < 1
    options = aioptions();
  end
  if ~isstruct(options) || ~isscalar(options)
    error('Nelson:ai:optionsAScalarStructReturned', 'options must be a scalar struct returned by aioptions.');
  end
  provider = lower(options.Provider);
  switch provider
    case 'ollama'
      response = listOllamaModels(options);
      models = extractOllamaModelNames(response);
    case 'openai-compatible'
      response = listOpenAICompatibleModels(options);
      models = extractOpenAICompatibleModelNames(response);
    otherwise
      error('Nelson:ai:unsupportedAIProvider', 'Unsupported AI provider.');
  end
  varargout{1} = models;
  if nargout > 1
    varargout{2} = response;
  end
end
%=============================================================================
function response = listOllamaModels(options)
  webOpts = weboptions();
  webOpts.RequestMethod = 'GET';
  webOpts.Timeout = options.Timeout;
  response = webread(ollamaTagsEndpoint(options.Endpoint), webOpts);
end
%=============================================================================
function response = listOpenAICompatibleModels(options)
  webOpts = weboptions();
  webOpts.RequestMethod = 'GET';
  webOpts.Timeout = options.Timeout;
  if isfield(options, 'TokenEnvVar') && ~isempty(options.TokenEnvVar)
    token = getenv(options.TokenEnvVar);
    if isempty(token)
      error('Nelson:ai:aIProviderTokenEnvironmentVariable', 'AI provider token environment variable is not defined.');
    end
    webOpts.HeaderFields = {'Authorization', ['Bearer ', token]};
  end
  response = webread(openAICompatibleModelsEndpoint(options.Endpoint), webOpts);
end
%=============================================================================
function endpoint = ollamaTagsEndpoint(endpoint)
  marker = strfind(endpoint, '/api/');
  if ~isempty(marker)
    endpoint = [endpoint(1:marker(1) - 1), '/api/tags'];
    return
  end
  while ~isempty(endpoint) && endpoint(end) == '/'
    endpoint(end) = [];
  end
  endpoint = [endpoint, '/api/tags'];
end
%=============================================================================
function endpoint = openAICompatibleModelsEndpoint(endpoint)
  marker = strfind(endpoint, '/v1/');
  if ~isempty(marker)
    endpoint = [endpoint(1:marker(1) + 2), '/models'];
    return
  end
  while ~isempty(endpoint) && endpoint(end) == '/'
    endpoint(end) = [];
  end
  endpoint = [endpoint, '/v1/models'];
end
%=============================================================================
function names = extractOllamaModelNames(response)
  names = {};
  if ~isstruct(response) || ~isfield(response, 'models')
    return
  end
  models = response.models;
  for k = 1:numel(models)
    if isfield(models(k), 'name')
      names{end + 1} = models(k).name;
    elseif isfield(models(k), 'model')
      names{end + 1} = models(k).model;
    end
  end
end
%=============================================================================
function names = extractOpenAICompatibleModelNames(response)
  names = {};
  if ~isstruct(response) || ~isfield(response, 'data')
    return
  end
  models = response.data;
  for k = 1:numel(models)
    if isfield(models(k), 'id')
      names{end + 1} = models(k).id;
    end
  end
end
%=============================================================================
