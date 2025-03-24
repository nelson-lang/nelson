%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = license(varargin)
  narginchk(0, 0);
  nLhs = nargout;
  if nLhs > 2
    error(_('Wrong number of output arguments.'));
  end
  if ispc()
    if ismodule('fftw')
      fft_file_info = dir([modulepath('nelson', 'builtin'), '/libfftw3-3.dll']);
      % fftw mkl wrapper > 20 Mo dll
      isFFTMKLWRAPPER = (fft_file_info.bytes > 20000000);
      isLGPL = isFFTMKLWRAPPER;
    else
      isLGPL = true;
    end
  else
    isLGPL = ~ismodule('fftw');
  end
  if isLGPL
    inUse = 'GNU Lesser General Public License v3.0';
    txt = fileread([nelsonroot(), '/lgpl-3.0.md']);
  else
    inUse = 'GNU General Public License v3.0';
    txt = fileread([nelsonroot(), '/gpl-3.0.md']);
  end
  if nLhs > 1
    if isLGPL
      txt = fileread([nelsonroot(), '/lgpl-3.0.md']);
    else
      txt = fileread([nelsonroot(), '/gpl-3.0.md']);
    end
  end
  if nLhs == 0
    disp(txt);
  else
    if nLhs == 1
      varargout{1} = inUse;
    else
      varargout{1} = inUse;
      varargout{2} = txt;
    end
  end
end
%=============================================================================
