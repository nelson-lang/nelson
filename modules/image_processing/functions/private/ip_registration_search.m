%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [bestT, bestAngle, bestScale] = ip_registration_search(moving, fixed, angles, scales, metricType, options)
  bestScore = -Inf;
  bestT = options.InitialTransformation;
  bestAngle = 0;
  bestScale = 1;
  for a = angles
    for s = scales
      candidateBase = options.InitialTransformation ...
        * ip_registration_similarity_matrix(size(moving), a, s);
      candidateWarped = imwarp(moving, candidateBase, 'Interpolation', options.Interpolation, ...
        'OutputView', [size(fixed, 1), size(fixed, 2)]);
      delta = imregcorr(candidateWarped, fixed);
      candidateT = candidateBase * delta.T;
      score = ip_registration_score(moving, fixed, candidateT, metricType, options.Interpolation);
      if score > bestScore
        bestScore = score;
        bestT = candidateT;
        bestAngle = a;
        bestScale = s;
      end
    end
  end
end
%=============================================================================
