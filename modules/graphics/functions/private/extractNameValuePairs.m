%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [s, remainingArgs, invalidMix] = extractNameValuePairs(varargin)
  
  % Initialize the output structure and remaining arguments
  s = struct();
  remainingArgs = {};
  invalidMix = false;  % Default to no invalid mix
  i = nargin; % Start from the last argument
  
  % Flag to check if we already encountered name-value pairs
  foundNameValuePairs = false;
  
  for i = nargin:-2:2
    % Get current name-value pair
    name = varargin{i - 1};
    value = varargin{i};
    
    % Check if the current name is a valid field name
    if (ischar(name) || isStringScalar(name)) && isValidFieldName(name)
      % If we have already found name-value pairs, set the invalid mix flag
      if foundNameValuePairs
        invalidMix = true;
      end
      % Add the name-value pair to the structure
      s.(char(name)) = value;
      foundNameValuePairs = true;  % We have found at least one name-value pair
    else
      % If we encounter a non-name argument after finding name-value pairs, set the invalid mix flag
      if foundNameValuePairs
        invalidMix = true;
      end
      remainingArgs = [{name} {value} remainingArgs];
    end  
  end
  
  if i > 2
    % If there are any remaining arguments that are not name-value pairs, add them to remainingArgs
    remainingArgs = [varargin(1:i-2), remainingArgs];
  end
end

%=============================================================================
% Helper function to check if a name could be a valid field name
function result = isValidFieldName(name)
  try
    if isStringScalar(name)
      name = char(name);
    end
    % Check if it's a valid variable name (this excludes invalid names like '+')
    result = isvarname(name);
  catch
    result = false;
  end
end
%=============================================================================
