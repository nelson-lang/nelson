%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function value = optimget(options, name, defaultValue)
  if nargin < 2
    error('Nelson:notEnoughInputs', 'Not enough input arguments.');
  end
  if nargin > 3
    error('Nelson:tooManyInputs', 'Too many input arguments.');
  end
  if nargin < 3
    defaultValue = [];
  end
  if isempty(options)
    value = defaultValue;
    return
  end
  if isobject(options)
    options = struct(options);
  end
  if ~isstruct(options) || ~isscalar(options)
    error('Nelson:optimization:invalidOptions', 'Options must be a scalar structure or optimization options object.');
  end
  name = optimCanonicalName(name);
  if isfield(options, name)
    value = options.(name);
    if isempty(value)
      value = defaultValue;
    end
  else
    value = defaultValue;
  end
end
%=============================================================================
