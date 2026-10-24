%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function validateSurfacePropertySyntax(inputArguments)
  firstString = find(cellfun('isclass', inputArguments, 'char'), 1);
  if isempty(firstString)
    return
  end
  propertiesList = inputArguments(firstString:end);
  if mod(length(propertiesList), 2) ~= 0
    error('Nelson:graphics:invalidNameValueArguments', 'Invalid parameter/value pair arguments.');
  end
  for k = 1:2:length(propertiesList)
    if ~ischar(propertiesList{k}) || ~graphicsIsPropertyNameToken(propertiesList{k}, 'surface')
      error('Nelson:graphics:invalidNameValueArguments', 'Invalid parameter/value pair arguments.');
    end
  end
end
%=============================================================================
