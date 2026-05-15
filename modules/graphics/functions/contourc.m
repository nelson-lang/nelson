%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function M = contourc(varargin)
  inputArguments = convertStringToCharArgs(varargin);
  nbArguments = length(inputArguments);
  if nbArguments < 1 || nbArguments > 4
    error(_('Wrong number of input arguments.'));
  end
  if (length(inputArguments{1}) == 1) && isgraphics(inputArguments{1}, 'axes')
    error(_('Axes input is not supported for contourc.'));
  end

  X = [];
  Y = [];
  levels = [];
  if nbArguments == 1
    Z = inputArguments{1};
  elseif nbArguments == 2
    Z = inputArguments{1};
    levels = inputArguments{2};
  elseif nbArguments == 3
    X = inputArguments{1};
    Y = inputArguments{2};
    Z = inputArguments{3};
  else
    X = inputArguments{1};
    Y = inputArguments{2};
    Z = inputArguments{3};
    levels = inputArguments{4};
  end

  validateContourData(X, Y, Z);
  if ~isempty(levels)
    levels = normalizeContourLevels(Z, levels);
  end
  M = __contourc__(Z, X, Y, levels);
end
%=============================================================================
