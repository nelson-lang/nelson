%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function val = statget(options, field, defaultData)
  narginchk(2, 3);
  nargoutchk(0, 1);

  if ~isstruct(options) || ~isscalar(options)
    error('Nelson:statistics:invalidOptions', '%s must be a scalar structure.', 'OPTIONS');
  end
  if ~isTextScalar(field)
    error('Nelson:validators:mustBeTextScalar', '%s must be a character vector or string scalar.', 'FIELD');
  end

  name = matchingField(options, char(field));
  if isempty(name)
    val = [];
    return
  end

  val = options.(name);
  if nargin == 3 && isempty(val)
    val = defaultData;
  end
end
%=============================================================================
function name = matchingField(options, field)
  name = '';
  fields = fieldnames(options);
  lowerField = lower(field);
  matches = {};
  for k = 1:length(fields)
    candidate = lower(fields{k});
    if strcmp(candidate, lowerField)
      name = fields{k};
      return
    end
    if strncmp(candidate, lowerField, length(lowerField))
      matches{end + 1} = fields{k};
    end
  end
  if length(matches) == 1
    name = matches{1};
  end
end
%=============================================================================
function tf = isTextScalar(value)
  tf = ischar(value) || isStringScalar(value);
end
%=============================================================================
