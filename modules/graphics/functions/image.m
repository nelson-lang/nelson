%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = image(varargin)
  inputArguments = convertStringToCharArgs(varargin);
  [parent, inputArguments] = imageParseParent(inputArguments);

  % image('CData', C, ...) is the low-level form: it adds the image to the
  % target axes as-is. Unlike the high-level forms it does not go through
  % newplot(), so the axes keeps its children and the limit, tick and
  % direction settings the caller pinned before the image was added.
  if (~isempty(inputArguments) && ischar(inputArguments{1}))
    [parent, inputArguments] = imageExtractParentProperty(parent, inputArguments);
    im = __image__('Parent', parent, inputArguments{:});
    if nargout == 1
      varargout{1} = im;
    end
    return
  end

  if isempty(parent)
    parent = graphicsParentProperty(inputArguments);
  end
  if isempty(parent)
    ax = newplot();
    parent = ax;
  elseif isgraphics(parent, 'hggroup') || isgraphics(parent, 'hgtransform')
    ax = ancestor(parent, 'axes');
  else
    ax = newplot(parent);
    parent = ax;
  end
  if (length(inputArguments) == 0)
    C = imageDemoCData();
    im = __image__('Parent', parent, 'CData', C, 'XData', [], 'YData', []);
    if nargout == 1
      varargout{1} = im;
    end
    return
  end
  ax.YDir = 'reverse';
  if ((length(inputArguments) == 1) || ((length(inputArguments) > 1) && (ischar(inputArguments{2}))))
    C = inputArguments{1};
    im = __image__('Parent', parent, 'CData', C, 'XData', [], 'YData', [], inputArguments{2:end});
    axis(ax, 'tight');
    ax.Box = 'on';
  elseif (numel(inputArguments) == 2)
    C = inputArguments{1};
    axis('maximal');
    im = __image__('Parent', parent, 'CData', C, 'XData', [], 'YData', []);
    if exist('zoom')
      zoom(inputArguments{2});
    end
  elseif (length(inputArguments) >= 3)
    x = inputArguments{1};
    y = inputArguments{2};
    C = inputArguments{3};
    inputArguments(1:3) = [];
    im = __image__('Parent', parent, 'XData', x, 'YData', y, 'CData', C, inputArguments{:});
    axis(ax, 'tight');
    ax.Box = 'on';
  else
    error('Nelson:graphics:invalidInputArguments', 'Invalid input arguments.');
  end
  if nargout == 1
    varargout{1} = im;
  end
end
%=============================================================================
function [parent, args] = imageExtractParentProperty(parent, args)
  % 'Parent' given as a property/value pair wins over a leading handle, and is
  % removed from the list so it is passed once. Only an explicit target avoids
  % gca(), which would otherwise create a figure the low-level form never uses.
  if (mod(numel(args), 2) ~= 0)
    error('Nelson:graphics:pvPairsExpected', 'Property/value pairs expected.');
  end
  parentIndex = [];
  for k = 1:2:numel(args)
    if (ischar(args{k}) && strcmpi(args{k}, 'Parent'))
      parentIndex = k;
    end
  end
  if ~isempty(parentIndex)
    parent = args{parentIndex + 1};
    args(parentIndex:parentIndex + 1) = [];
  elseif isempty(parent)
    parent = gca();
  end
end
%=============================================================================
function [parent, args] = imageParseParent(args)
  parent = [];
  if ~isempty(args) && isscalar(args{1}) && (isgraphics(args{1}, 'axes') || isgraphics(args{1}, 'hggroup') || isgraphics(args{1}, 'hgtransform'))
    parent = args{1};
    args = args(2:end);
  end
end
%=============================================================================
