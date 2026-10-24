%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function pairs = __nelsonc_option_pairs__(names, pairs, allowStruct)
  if allowStruct && numel(pairs) == 1 && isstruct(pairs{1}) && isscalar(pairs{1})
    values = pairs{1};
    fields = fieldnames(values);
    pairs = {};
    for k = 1:numel(fields)
      pairs(end + 1:end + 2) = {fields{k}, values.(fields{k})};
    end
  end
  if mod(numel(pairs), 2) ~= 0
    if allowStruct
      error('Nelson:nelsonc:options:buildOptionsRequireNameValueOrStruct', 'Build options require name-value pairs or a scalar struct.');
    end
    error('Nelson:nelsonc:options:buildOptionsRequireNameValuePairs', 'Build options require name-value pairs.');
  end
  for k = 1:2:numel(pairs)
    name = pairs{k};
    if isstring(name) && isscalar(name)
      name = char(name);
    end
    if ~ischar(name) || ~isrow(name) || isempty(name)
      error('Nelson:nelsonc:options:buildOptionNamesCharacterVectors', 'Build option names must be character vectors or scalar strings.');
    end
    index = find(strcmpi(names, name), 1);
    if isempty(index) || strcmpi(name, 'AppFile')
      error('Nelson:nelsonc:options:unknownUnsupportedBuildOption', 'Unknown or unsupported build option: %s.', name);
    end
    pairs{k} = names{index};
  end
end
