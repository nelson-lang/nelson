%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function value = __predictor_names__(value, p)
  if isstring(value)
    value = cellstr(value(:));
  elseif iscellstr(value)
    value = value(:);
  else
    error('Nelson:validators:mustBeTextArray', '%s must be a string array or cell array of character vectors.', 'PredictorNames');
  end
  if length(value) ~= p
    error('Nelson:statistics:predictorNamesOneNamePredictor', 'PredictorNames must have one name per predictor.');
  end
end
%=============================================================================
