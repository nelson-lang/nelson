%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = animatedline(varargin)
  nargoutchk(0, 1);
  inputArguments = convertStringToCharArgs(varargin);
  [parent, inputArguments] = parseParent(inputArguments);
  [x, y, z, propertiesList, hasInitialPoints] = parseAnimatedLineArguments(inputArguments);
  validatePropertyPairs(propertiesList);
  h = __animatedline__('Parent', parent, propertiesList{:});
  if hasInitialPoints
    addpoints(h, x, y, z);
  end
  if nargout > 0
    varargout{1} = h;
  end
end
%=============================================================================
function [parent, args] = parseParent(args)
  if ~isempty(args) && isscalar(args{1}) && (isgraphics(args{1}, 'axes') || isgraphics(args{1}, 'hggroup'))
    parent = args{1};
    args = args(2:end);
  else
    parent = gca();
  end
end
%=============================================================================
function [x, y, z, propertiesList, hasInitialPoints] = parseAnimatedLineArguments(args)
  x = [];
  y = [];
  z = [];
  hasInitialPoints = false;
  numericCount = 0;
  while numericCount < numel(args) && isnumeric(args{numericCount + 1})
    numericCount = numericCount + 1;
  end
  if numericCount == 0
    propertiesList = args;
    return
  end
  if numericCount ~= 2 && numericCount ~= 3
    error('Nelson:graphics:invalidInputArguments', 'Invalid input arguments.');
  end
  x = args{1};
  y = args{2};
  if numericCount == 3
    z = args{3};
  else
    z = zeros(size(x));
  end
  propertiesList = args(numericCount + 1:end);
  hasInitialPoints = true;
end
%=============================================================================
function validatePropertyPairs(propertiesList)
  if rem(numel(propertiesList), 2) ~= 0
    error('Nelson:graphics:nameValuePair', 'Name-value arguments must be paired.');
  end
end
%=============================================================================
