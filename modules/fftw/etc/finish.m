%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
FFTWwrapper('free');
if contains(path(), modulepath('fftw', 'functions'))
  rmpath(modulepath('fftw', 'functions'));
end
removegateway(modulepath('fftw', 'builtin'));
%=============================================================================
