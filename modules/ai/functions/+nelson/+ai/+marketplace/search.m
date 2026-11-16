%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Search the skill catalog (see catalogUrl). Returns a struct array with fields
% name, description and url. An empty query returns the whole catalog; an
% unreachable catalog returns an empty result rather than erroring.
function result = search(query)
  narginchk(0, 1);
  nargoutchk(0, 1);
  if nargin < 1
    query = '';
  end
  query = char(query);
  result = struct('name', {}, 'description', {}, 'url', {});
  try
    data = webread(catalogUrl());
  catch
    return
  end
  entries = normalizeCatalog(data);
  for k = 1:numel(entries)
    entry = entries(k);
    if isempty(query) || matchesQuery(entry, query)
      result(end + 1) = entry; %#ok<AGROW>
    end
  end
end
%=============================================================================
function entries = normalizeCatalog(data)
  entries = struct('name', {}, 'description', {}, 'url', {});
  items = {};
  if iscell(data)
    items = data;
  elseif isstruct(data)
    for k = 1:numel(data)
      items{end + 1} = data(k); %#ok<AGROW>
    end
  end
  for k = 1:numel(items)
    item = items{k};
    if ~isstruct(item) || ~isfield(item, 'name') || ~isfield(item, 'url')
      continue
    end
    description = '';
    if isfield(item, 'description')
      description = char(item.description);
    end
    entries(end + 1) = struct('name', char(item.name), ...
      'description', description, 'url', char(item.url)); %#ok<AGROW>
  end
end
%=============================================================================
function tf = matchesQuery(entry, query)
  q = lower(query);
  tf = contains(lower(entry.name), q) || contains(lower(entry.description), q);
end
%=============================================================================
