%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [Vr, Dr] = cdf2rdf(V, D)
  % complex eigen data is not supported on the device, so the conversion runs on the host
  narginchk(2, 2);
  [Vr, Dr] = __gpuArray_host__('cdf2rdf', V, D);
end
%=============================================================================
