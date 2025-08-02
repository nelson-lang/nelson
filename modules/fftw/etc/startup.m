%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
addgateway(modulepath('fftw', 'builtin'), 'fftw');
if FFTWwrapper('load')
  addpath(modulepath('fftw', 'functions'), '-frozen');
else
  warning('Nelson:fftw:loadingFails', _('library fftw not loaded.'))
  removemodule('fftw');
end
%=============================================================================
