%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = surface(varargin)
  % surface(X, Y, Z)
  % surface(X, Y, Z, C)
  % surface(Z)
  % surface(Z, C)
  % surface(ax, ...) 
  nargoutchk(0, 1);
  
  inputArguments = varargin;
  nbInputArguments = nargin;
  if (nbInputArguments >= 2)
    if ((length(inputArguments{1}) == 1) && isgraphics(inputArguments{1}, 'axes'))
      go = inputArguments{1}(1);
      inputArguments = inputArguments(2:end);
      nbInputArguments = length(inputArguments);
    else   
      go = newplot();
    end
  else
    go = newplot();
  end
  saveca = gca();
  axes(go);
  propertiesList = {};
  firstString = find (cellfun ('isclass', inputArguments, 'char'), 1);
  if (isempty(firstString))
   firstString = nbInputArguments + 1;
  end
  propertiesList = inputArguments(firstString:end);
  inputArguments = inputArguments(1:firstString-1);
 
  if (length(inputArguments) == 0)
    X = 1:3;
    Y = X';
    C = eye (3);
    Z = eye (3);

    h = __surf__('ZData', Z, propertiesList);
    xlim(go, [1 3]);
    ylim(go, [1 3]);
  elseif (length(inputArguments) == 1)
    if isempty(propertiesList)
      h = __surf__('ZData', inputArguments{1}, propertiesList);
    else
      h = __surf__('ZData', inputArguments{1}, propertiesList{:});
    end
  elseif (length(inputArguments) == 3)
    h = __surf__('XData', inputArguments{1}, ...
    'YData',inputArguments{2}, ...
    'ZData',inputArguments{3}, propertiesList{:});
  elseif (length(inputArguments) == 4)
    if isempty(propertiesList)
      h = __surf__('XData',inputArguments{1}, ...
      'YData', inputArguments{2}, ...
      'ZData',inputArguments{3}, ...
      'CData',inputArguments{4}, propertiesList);
    else
      h = __surf__('XData',inputArguments{1}, ...
      'YData', inputArguments{2}, ...
      'ZData',inputArguments{3}, ...
      'CData',inputArguments{4}, ...
      propertiesList{:});
    end
  else
    error(_('Invalid parameter/value pair arguments.'));
  end
  axes(saveca);
  grid(go, 'on')
  grid(go, 'off')
  if nargout > 0
    varargout{1} = h;
  end

end
