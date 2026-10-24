%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function levels = normalizeContourLevels(Z, levelsArgument)
  if isempty(levelsArgument)
    levels = [];
    return
  end
  if ~isnumeric(levelsArgument) || ~isvector(levelsArgument)
    error('Nelson:normalizeContourLevels:invalidContourLevels', 'Contour levels must be a numeric scalar or vector.');
  end
  levelsArgument = double(levelsArgument(:)');
  if numel(levelsArgument) == 1
    n = levelsArgument(1);
    if ~isfinite(n) || n < 1 || floor(n) ~= n
      error('Nelson:graphics:invalidInteger', '%s must be a positive integer scalar.', 'Contour level count');
    end
    finiteZ = Z(isfinite(Z));
    if isempty(finiteZ)
      levels = [];
    elseif min(finiteZ(:)) == max(finiteZ(:))
      % A flat range holds a single level whatever the count asked for: the
      % value the data sits at.
      levels = double(min(finiteZ(:)));
    else
      ticks = linspace(min(finiteZ(:)), max(finiteZ(:)), n + 2);
      levels = ticks(2:end - 1);
    end
  else
    if any(~isfinite(levelsArgument))
      error('Nelson:normalizeContourLevels:contourLevelsNotFinite', 'Contour levels must be finite values.');
    end
    if numel(levelsArgument) == 2 && levelsArgument(1) == levelsArgument(2)
      levels = levelsArgument(1);
    else
      if any(diff(levelsArgument) <= 0)
        error('Nelson:normalizeContourLevels:contourLevelsNotIncreasing', 'Contour levels must be monotonically increasing.');
      end
      levels = levelsArgument;
    end
  end
end
%=============================================================================
