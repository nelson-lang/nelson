%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function y = interpft(x, ny, dim)
  narginchk(2, 3);
  if nargin == 3
    perm = [dim, 1:dim - 1, dim + 1:ndims(x)];
    y = ipermute(interpftColumns(permute(x, perm), ny), perm);
    return
  end
  [xs, nshifts] = shiftdim(x);
  y = shiftdim(interpftColumns(xs, ny), -nshifts);
end
%=============================================================================
function y = interpftColumns(x, ny)
  sampleCount = size(x, 1);
  if ny == sampleCount
    y = x;
    return
  end
  if ny < sampleCount
    error('Nelson:interpft:tooFewOutputPoints', 'The number of output points must be at least the number of input points.');
  end
  columnCount = size(x, 2);
  spectrum = fft(x, [], 1);
  splitIndex = ceil((sampleCount + 1) / 2);
  paddedSpectrum = [spectrum(1:splitIndex, :); ...
                    zeros(ny - sampleCount, columnCount); ...
                    spectrum(splitIndex + 1:sampleCount, :)];
  if rem(sampleCount, 2) == 0
    paddedSpectrum(splitIndex, :) = paddedSpectrum(splitIndex, :) / 2;
    paddedSpectrum(splitIndex + ny - sampleCount, :) = paddedSpectrum(splitIndex, :);
  end
  y = ifft(paddedSpectrum, [], 1);
  if isreal(x)
    y = real(y);
  end
  y = y * ny / sampleCount;
end
%=============================================================================
