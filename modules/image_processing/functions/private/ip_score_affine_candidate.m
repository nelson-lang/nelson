%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [score, candidateT] = ip_score_affine_candidate(moving, fixed, angle, scaleX, scaleY, shear, metricType, options)
  candidateBase = options.InitialTransformation ...
    * ip_registration_affine_matrix(size(moving), angle, scaleX, scaleY, shear);
  candidateWarped = imwarp(moving, candidateBase, 'Interpolation', options.Interpolation, ...
    'OutputView', [size(fixed, 1), size(fixed, 2)]);
  delta = imregcorr(candidateWarped, fixed);
  candidateT = candidateBase * delta.T;
  score = ip_registration_score(moving, fixed, candidateT, metricType, options.Interpolation);
end
%=============================================================================
