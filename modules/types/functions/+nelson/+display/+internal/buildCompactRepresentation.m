%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Builds a nelson.display.CompactDisplayRepresentation from a data description.
% Backs the widthConstrainedDataRepresentation / fullDataRepresentation helper
% methods of nelson.mixin.CustomCompactDisplayProvider.
%   config : a nelson.display.DisplayConfiguration
%   width  : maximum number of characters (Inf for the full representation)
%   the remaining name-value arguments give the data:
%     'StringArray', strings   or   'CellArrayOfCharVectors', cellOfChars
%=============================================================================
function rep = buildCompactRepresentation(config, width, varargin)
  items = {};
  k = 1;
  while k < numel(varargin)
    name = varargin{k};
    value = varargin{k + 1};
    switch char(name)
      case 'StringArray'
        value = string(value);
        items = cellstr(value(:));
      case 'CellArrayOfCharVectors'
        items = value(:);
      case 'PaddedDisplayOutput'
        items = {char(value)};
    end
    k = k + 2;
  end
  delimiter = config.InterElementDelimiter;
  joined = strjoin(cellfun(@char, items(:).', 'UniformOutput', false), delimiter);
  if ~isinf(width) && numel(joined) > width
    if width > 1
      joined = [joined(1:(width - 1)), config.Ellipsis];
    else
      joined = config.Ellipsis;
    end
  end
  rep = nelson.display.CompactDisplayRepresentation(joined);
end
%=============================================================================
