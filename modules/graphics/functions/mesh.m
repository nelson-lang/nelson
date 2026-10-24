%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = mesh(varargin)
  narginchk(1, 1000);
  nargoutchk(0, 1);

  inputArguments = convertStringToCharArgs(varargin);
  nbInputArguments = length(inputArguments);
  if (nbInputArguments >= 2) && (length(inputArguments{1}) == 1) && isgraphics(inputArguments{1}, 'axes')
    go = newplotTarget(inputArguments{1}(1));
    inputArguments = inputArguments(2:end);
  else
    go = newplotTarget(graphicsParentProperty(inputArguments));
  end

  firstProperty = find(cellfun('isclass', inputArguments, 'char'), 1);
  if isempty(firstProperty)
    firstProperty = length(inputArguments) + 1;
  end
  propertiesList = inputArguments(firstProperty:end);
  dataArguments = inputArguments(1:firstProperty - 1);
  defaultProperties = {'FaceColor', 'w', 'EdgeColor', 'flat', 'FaceLighting', 'none', 'EdgeLighting', 'flat'};
  % Concatenate at the cell level: expanding the comma list here would build a
  % matrix out of the data, the property names and the handles, which fails as
  % soon as a name/value pair is given. The defaults come before the caller
  % properties so that a property set by the caller wins over the mesh default.
  args = [dataArguments, defaultProperties, propertiesList];
  h = surface(go, args{:});

  % Adding to a group leaves the axes untouched: newplot did not start it anew,
  % so the 3-D framing is not applied either.
  if (~isgraphics(go, 'hggroup') && ~ishold(go))
    view(go, 3);
    grid(go, 'on');
  end
  if nargout > 0
    varargout{1} = h;
  end
end
%=============================================================================
