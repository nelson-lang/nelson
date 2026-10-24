%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function y = fft2(x, r, c)
  % 2-D fast Fourier transform
  % y = fft2(X)
  % Y = fft2(X, m, n)
  inputCount = nargin();
  if inputCount < 1
    error('Nelson:notEnoughInputs', 'Not enough input arguments.');
  end
  if inputCount == 2
    error('Nelson:wrongNumberOfInputs', 'Wrong number of input arguments.');
  end
  nargoutchk(0, 1);
  if inputCount == 1
    y = transformFirstTwoDimensions(x, [], []);
  else
    y = transformFirstTwoDimensions(x, r, c);
  end
end
%=============================================================================
function y = transformFirstTwoDimensions(x, rowCount, columnCount)
  if isempty(columnCount)
    transformedColumns = fft(x, [], 2);
  else
    transformedColumns = fft(x, columnCount, 2);
  end
  if isempty(rowCount)
    y = fft(transformedColumns, [], 1);
  else
    y = fft(transformedColumns, rowCount, 1);
  end
end
%=============================================================================
