%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function ref = odeReferenceValues()
  ref.ode45Time = [0; 0.25; 0.5];
  ref.ode45Exp = [1; 1.2840254168690106; 1.6487212709656938];

  ref.ode23Time = [0; pi / 4];
  ref.ode23Oscillator = [0 1; 0.70710677869847 0.70710677867768712];

  ref.reverseTime = [0.5; 0.25; 0];
  ref.reverseExp = [1.6487212707001282; 1.2840254167762307; 1.0000000000333491];

  ref.eventTwoTime = [0.25; 0.75];
  ref.eventTwoSolution = [0.25; 0.75];
  ref.eventTwoIndex = [1; 2];

  ref.massConstant = 1.2840254166877414;
  ref.massFunction = 1.2840254167020773;
  ref.implicitHalf = 0.60653065971263342;

  ref.objectSolutionHalf = 0.60653066070931139;
  ref.objectInterpolationQuarter = 0.77880078307140488;

  ref.devalTimes = [0 0.25 0.5];
  ref.devalExp = [1 exp(0.25) exp(0.5)];

  ref.highOrderTimes = [0 0.125 0.25];
  ref.highOrderExp = [1; 1.1331484530668263; 1.2840254166877414];

  ref.massTime = [0; 0.25; 0.5];
  ref.massTimeSolution = [1; 1.25; 1.5];
end
%=============================================================================
