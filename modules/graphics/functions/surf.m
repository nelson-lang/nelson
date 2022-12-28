%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = surf(varargin)
  narginchk(1, 1000);
  nargoutchk(0, 1);
  
  inputArgumrents = varargin;
  nbInputArguments = nargin;
  if (nbInputArguments >= 2)
    if ((length(inputArgumrents{1}) == 1) && isgraphics(inputArgumrents{1},'axes'))
      go = inputArgumrents{1}(1);
      inputArgumrents(1) = [];
      nbInputArguments = nbInputArguments - 1;
    else   
      go = newplot();
    end
  else
    go = newplot();
  end
  nextplot = go.NextPlot;
  
  saveca = gca();
  axes(go);
  propertyIndex = 0;
  if (nbInputArguments > 2)
    propertyIndex = nbInputArguments - 1;
    while ((propertyIndex >= 1) && ischar(inputArgumrents{propertyIndex}) && isValidGraphicsProperty('line', inputArgumrents{propertyIndex}))
      propertyIndex = propertyIndex - 2;
    end
    propertyIndex = propertyIndex + 2;
  end
  propertiesList = {};
  if ((propertyIndex > 0) & (propertyIndex < nbInputArguments))
    propertiesList = inputArgumrents(propertyIndex:end);
    inputArgumrents(propertyIndex:end) = [];
  end
  if (length(inputArgumrents) == 0)
    h = __surf__(propertiesList{:});
  elseif (length(inputArgumrents) == 1)
    if isempty(propertiesList)
      h = __surf__('ZData', inputArgumrents{1}, propertiesList);
    else
      h = __surf__('ZData', inputArgumrents{1}, propertiesList{:});
    end
  elseif (length(inputArgumrents) == 3)
    h = __surf__('XData', inputArgumrents{1}, ...
    'YData',inputArgumrents{2}, ...
    'ZData',inputArgumrents{3}, propertiesList{:});
  elseif (length(inputArgumrents) == 4)
    if isempty(propertiesList)
      h = __surf__('XData',inputArgumrents{1}, ...
      'YData', inputArgumrents{2}, ...
      'ZData',inputArgumrents{3}, ...
      'CData',inputArgumrents{4}, propertiesList);
    else
      h = __surf__('XData',inputArgumrents{1}, ...
      'YData', inputArgumrents{2}, ...
      'ZData',inputArgumrents{3}, ...
      'CData',inputArgumrents{4}, ...
      propertiesList{:});
    end
  else
    error(_('Invalid parameter/value pair arguments.'));
  end
  axes(saveca);
  
  if strcmp(nextplot, 'replaceall') || strcmp(nextplot, 'replace') 
    view(3);
    grid('on');
  elseif strcmp(nextplot, 'replacechildren')
    view(3);
  end
  
  if nargout > 0
    varargout{1} = h;
  end
end
%=============================================================================
