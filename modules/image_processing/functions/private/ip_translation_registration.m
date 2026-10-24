%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function T = ip_translation_registration(moving, fixed, options)
  warped = imwarp(moving, options.InitialTransformation, 'Interpolation', options.Interpolation, ...
    'OutputView', [size(fixed, 1), size(fixed, 2)]);
  delta = imregcorr(warped, fixed);
  T = options.InitialTransformation * delta.T;
end
%=============================================================================
