%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [bestT, bestAngle, bestScaleX, bestScaleY, bestShear] = ...
  ip_registration_affine_search(moving, fixed, angles, options, metricType, varargin)
  if nargin == 5
    scalesX = unique([1 - options.ScaleSearch, 1, 1 + options.ScaleSearch]);
    scalesY = scalesX;
    shears = unique([-options.ShearSearch, 0, options.ShearSearch]);
  else
    scalesX = varargin{1};
    scalesY = varargin{2};
    shears = varargin{3};
  end
  bestScore = -Inf;
  bestT = options.InitialTransformation;
  bestAngle = 0;
  bestScaleX = 1;
  bestScaleY = 1;
  bestShear = 0;
  for a = angles
    for sx = scalesX
      for sy = scalesY
        for sh = shears
          [score, candidateT] = ip_score_affine_candidate(...
            moving, fixed, a, sx, sy, sh, metricType, options);
          if score > bestScore
            bestScore = score;
            bestT = candidateT;
            bestAngle = a;
            bestScaleX = sx;
            bestScaleY = sy;
            bestShear = sh;
          end
        end
      end
    end
  end
end
%=============================================================================
