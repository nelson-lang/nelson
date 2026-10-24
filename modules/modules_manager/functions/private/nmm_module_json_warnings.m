%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Single source of truth for module.json best-practice warnings.
% Returns non-blocking suggestions for recommended (but not required)
% metadata fields. Reused by nmm_validate ('-warnings') and nmm_init so the
% same advice is surfaced when authoring and when validating a manifest.
%=============================================================================
function warnings = nmm_module_json_warnings(data)
  warnings = {};
  recommended = {'description', 'repository', 'authors', 'keywords'};
  for k = 1:length(recommended)
    field = recommended{k};
    if isMissingField(data, field)
      warnings{end + 1} = ['module.json field is missing: ', field];
    end
  end
end
%=============================================================================
function tf = isMissingField(data, fieldname)
  tf = true;
  if ~isfield(data, fieldname)
    return
  end
  value = data.(fieldname);
  if isstring(value)
    value = cellstr(value);
  end
  if ischar(value)
    tf = isempty(strtrim(value));
  elseif iscell(value)
    tf = isempty(value);
  elseif isstruct(value)
    tf = isempty(fieldnames(value));
  else
    tf = isempty(value);
  end
end
%=============================================================================
