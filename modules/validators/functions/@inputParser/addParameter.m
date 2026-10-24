%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function addParameter(obj, name, defaultValue, validator)
  narginchk(3, 4);
  if nargin < 4
    validator = [];
  end
  if ~(isempty(validator) || isa(validator, 'function_handle'))
    error('Nelson:inputParser:validatorAFunctionHandleEmpty4', 'Validator must be a function handle or empty.');
  end
  inputParser.addSchema(obj, 'parameter', name, defaultValue, validator);
end
%=============================================================================
