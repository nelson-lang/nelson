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
  if ~(nargin() == 1 || nargin() == 3)
    error(_('Wrong number of input arguments.'));
  end
  nargoutchk(0, 1);
  if ~ismatrix(x)
    if nargin == 1
      y = fft(fft(x, [], 2), [], 1);
    else
      y = fft(fft(x, c, 2), r, 1);
    end
  else
    if nargin==1
      y = fftn(x);
    else
      y = fftn(x, [r, c]);
    end
  end   
  %=============================================================================
  