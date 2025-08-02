%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function T = struct2table(inputStruct)
  narginchk(1, 1);
  nargoutchk(0, 1);
  
  % Ensure the input is a struct
  validateStructInput(inputStruct);
  varNames = fieldnames(inputStruct);
  if (isscalar(inputStruct))
    varValues = struct2cell(inputStruct);
    T = table (varValues{:}, 'VariableNames', varNames);
  else
    inputStruct = inputStruct(:);
    c = struct2cell(inputStruct);
    c = c';
    T = cell2table (c, 'VariableNames', varNames);
  end
end
%=============================================================================
function validateStructInput(inputStruct)
  % Validate that input is a structure
  if ~isstruct(inputStruct)
    error('Input must be a structure.');
  end
end
%=============================================================================
