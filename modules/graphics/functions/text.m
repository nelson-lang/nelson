%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = text(varargin)
  % h = text(x, y, label)
  % h = text(x, y, z, label)
  % h = text(ax, x, y, label)
  % h = text(ax, x, y, z, label)
  
  narginchk(3, 10000);
  nargoutchk(0, 1);
  inputArguments = varargin;
  if (isscalar(inputArguments{1}) && (isgraphics(inputArguments{1}, 'axes') || isgraphics(inputArguments{1}, 'hggroup')))
    parent = inputArguments{1};
    inputArguments = inputArguments(2:end);
  else
    parent = gca();
  end
  
  if isnumeric(inputArguments{1})
    x = inputArguments{1};
    inputArguments = inputArguments(2:end);
  else
    error(_('Invalid input argument: numeric value expected.'))
  end
  if isnumeric(inputArguments{1})
    y = inputArguments{1};
    inputArguments = inputArguments(2:end);
  else
    error(_('Invalid input argument: numeric value expected.'))
  end
  if isnumeric(inputArguments{1})
    z = inputArguments{1};
    inputArguments = inputArguments(2:end);
  else 
    z = zeros(size (x));
  end

  if (length(x) ~= length(y))
    error(_('Vectors x and y must be the same length.'));
  end
  if (length(x) ~= length(z))
    error(_('Vectors x and z must be the same length.'));
  end
  
  if numel(inputArguments) == 0
    error(_('last argument must be a string or cell of strings.'))
  end
  labelArray = [];
  if ischar(inputArguments{1})
    labelArray = charToLabel(inputArguments{1}, length(x));
  elseif iscellstr(inputArguments{1})
    labelArray = cellStringToLabel(inputArguments{1}, length(x));
  elseif iscell(inputArguments{1})
    labelArray = cellToLabel(inputArguments{1}, length(x));
  elseif isstring(inputArguments{1})
    labelArray = stringToLabel(inputArguments{1}, length(x));
  else
    error(_('last argument must be a string or cell of strings.'))
  end
  inputArguments = inputArguments(2:end);
  inputAsStruct = struct(inputArguments{:});
  if isfield(inputAsStruct, 'Parent')
    parent = inputAsStruct.Parent;
    rmfield(inputAsStruct, 'Parent');
  end
  inputArguments = reshape([fieldnames(inputAsStruct)'; struct2cell(inputAsStruct)'], 1, []);
  visibility = parent.Visible;
  H = [];
  for (i=1:numel(x))
    H = [H, __text__('Parent', parent, 'Visible', visibility, 'Position', [x(i), y(i), z(i)], 'String', labelArray{i}, inputArguments{1:end})];
  end
  if (nargout == 1)
    varargout{1} = H;
  end
end
%=============================================================================
function labelArray = charToLabel(value, len)
  label = value;
  labelArray = repmat({label}, [len, 1]);
end
%=============================================================================
function labelArray = cellStringToLabel(value, len)
  cellstr = value;
  label = '';
  nbElements = numel(cellstr);
  for idx = 1:nbElements
    if idx < nbElements
      label = [label, cellstr{idx}, char(10)];
    else 
      label = [label, cellstr{idx}];
    end
  end
  labelArray = repmat({label}, [len, 1]);
end
%=============================================================================
function labelArray = stringToLabel(value, len)
  if isscalar(varargin{indexArguments})
    label = varargin{indexArguments};
    if ismissing(label)
      label = '';
    else
      label = label{1};
    end
    labelArray = repmat({label}, [len, 1]);
  else
    strMat = varargin{indexArguments};
    label = '';
    nbElements = numel(strMat);
    for idx = 1:nbElements
      v = strMat{idx};
      if ismissing(v)
        v = '';
      else
        v = v{1};
      end
      if idx < nbElements
        label = [label, v, char(10)];
      else 
        label = [label, v];
      end
    end
    labelArray = repmat({label}, [len, 1]);
  end
  %=============================================================================
function labelArray = cellToLabel(value, len)
  cellstr = value;
  fnum = @isnumeric;
  idx = cellfun(fnum, value);
  fcell = @(x) sprintf ('%g', x);
  cellstr(idx) = cellfun (fcell, value(idx), 'UniformOutput', false);
  labelArray = cellStringToLabel(cellstr, len);
end
%=============================================================================
