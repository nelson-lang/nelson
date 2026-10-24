%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function value = bvpget(options, name, defaultValue)
  narginchk(2, 3);
  if nargin < 3
    activeDefaultValue = [];
  else
    activeDefaultValue = defaultValue;
  end
  if isempty(options)
    value = activeDefaultValue;
    return
  end
  if ~isstruct(options)
    error('Nelson:ode_solvers:bvpOptionsStructureExpected', '%s options structure expected.', 'BVP');
  end
  fieldName = bvpOptionName(name);
  if isfield(options, fieldName) && ~isempty(options.(fieldName))
    value = options.(fieldName);
  else
    value = activeDefaultValue;
  end
end
%=============================================================================
