%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function web_figure_navigate_2d(ax, request)
  zooming = strcmp(request.action, 'zoom');
  region = zooming && isfield(request, 'regionXFrac') && isfield(request, 'regionYFrac');
  hasFocus = false;
  if zooming && ~region
    factor = request.factor;
    if ~isnumeric(factor) || ~isscalar(factor) || ~isreal(factor) || ~isfinite(factor) || factor <= 0
      error('Nelson:graphics:invalidZoomFactor', 'Zoom factor must be a positive real scalar.');
    end
    % Optional focal point (fraction of the cursor inside the axes rectangle),
    % so wheel zoom keeps the point under the cursor fixed. Buttons omit it and
    % fall back to zooming about the view centre.
    hasFocus = isfield(request, 'focusXFrac') && isfield(request, 'focusYFrac') && ...
      isnumeric(request.focusXFrac) && isnumeric(request.focusYFrac) && ...
      isscalar(request.focusXFrac) && isscalar(request.focusYFrac) && ...
      isfinite(request.focusXFrac) && isfinite(request.focusYFrac);
  elseif ~zooming
    offsets = [-request.dxFrac request.dyFrac];
  end
  args = {};
  for k = 1:2
    dimension = char('X' + k - 1);
    limits = get(ax, [dimension 'Lim']);
    logarithmic = strcmp(get(ax, [dimension 'Scale']), 'log');
    if logarithmic
      limits = log10(limits);
    end
    reversed = strcmp(get(ax, [dimension 'Dir']), 'reverse');
    span = limits(2) - limits(1);
    if region
      % Rubber-band box, expressed as fractions of the axes rectangle. X runs
      % left->right (= increasing data), screen Y runs top->bottom so the data
      % fraction is 1 - yFrac; a reversed axis flips the mapping.
      if k == 1
        g = [request.regionXFrac(1), request.regionXFrac(2)];
      else
        g = [1 - request.regionYFrac(1), 1 - request.regionYFrac(2)];
      end
      g = min(max(g, 0), 1);
      if reversed
        g = 1 - g;
      end
      limits = sort([limits(1) + g(1) * span, limits(1) + g(2) * span]);
    elseif zooming
      if hasFocus
        if k == 1
          frac = request.focusXFrac;
        else
          frac = 1 - request.focusYFrac;
        end
        frac = min(max(frac, 0), 1);
        if reversed
          frac = 1 - frac;
        end
        anchor = limits(1) + frac * span;
      else
        anchor = mean(limits);
      end
      limits = anchor + (limits - anchor) / factor;
    else
      direction = 1;
      if reversed
        direction = -1;
      end
      limits = limits + direction * offsets(k) * span;
    end
    if logarithmic
      limits = 10 .^ limits;
    end
    args = [args, {[dimension 'Lim'], limits, [dimension 'LimMode'], 'manual'}];
  end
  set(ax, args{:});
end
