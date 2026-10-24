%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [X, map] = rgb2ind(RGB, arg, dither_option)
  narginchk(2, 3);
  nargoutchk(0, 2);
  if ~ip_is_rgb_image(RGB)
    error('Nelson:image_processing:rgbImageMNArray', '%s must be an M-by-N-by-3 array.', 'RGB');
  end
  doDither = true;
  if nargin == 3
    mustBeTextScalar(dither_option, 3);
    dither_option = convertStringsToChars(dither_option);
    switch lower(dither_option)
      case 'dither'
        doDither = true;
      case 'nodither'
        doDither = false;
      otherwise
        error('Nelson:rgb2ind:invalidDither', 'DITHER_OPTION must be ''dither'' or ''nodither''.');
    end
  end
  [rows, cols, ~] = size(RGB);
  if isempty(RGB)
    map = [0 0 0];
    X = zeros(rows, cols, 'uint8');
    return;
  end
  I = ip_to_unit_double(RGB);
  I = min(max(I, 0), 1);
  paletteGiven = false;
  if isscalar(arg) && (isnumeric(arg) || islogical(arg))
    a = double(arg);
    if ~isfinite(a)
      error('Nelson:rgb2ind:invalidN', 'N must be an integer >= 1 or a tolerance in the interval (0, 1).');
    end
    if a >= 1 && a == floor(a)
      mode = 'minvar';
      ncolors = a;
    elseif a > 0 && a < 1
      mode = 'uniform';
      tol = a;
    else
      error('Nelson:rgb2ind:invalidN', 'N must be an integer >= 1 or a tolerance in the interval (0, 1).');
    end
  elseif ip_is_colormap(arg)
    paletteGiven = true;
    map = min(max(double(arg), 0), 1);
  else
    error('Nelson:rgb2ind:invalidArg', 'Second argument must be a scalar number of colors, a scalar tolerance, or an M-by-3 colormap.');
  end
  P = reshape(I, rows * cols, 3);
  if ~paletteGiven
    switch mode
      case 'minvar'
        map = ip_median_cut_map(P, ncolors);
      case 'uniform'
        map = ip_uniform_map(P, tol);
    end
  end
  if doDither
    idx = ip_dither_assign(I, map);
  else
    idx = reshape(ip_nearest_assign(P, map), rows, cols);
  end
  if size(map, 1) <= 256
    X = uint8(idx);
  else
    X = uint16(idx);
  end
end
%=============================================================================
