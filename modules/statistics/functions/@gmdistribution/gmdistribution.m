%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef gmdistribution
  properties
    mu = zeros(0, 0)
    Sigma = zeros(0, 0, 0)
    ComponentProportion = zeros(1, 0)
    NumComponents = 0
    NumVariables = 0
    NegativeLogLikelihood = NaN
    AIC = NaN
    BIC = NaN
    Converged = false
    NumIterations = 0
    RegularizationValue = 0
    CovarianceType = 'full'
    SharedCovariance = false
  end

  methods
    function obj = gmdistribution(mu, Sigma, p)
      if nargin == 0
        return
      end
      if nargin < 3
        p = [];
      end
      [mu, Sigma, p] = __gmm_check_model__(mu, Sigma, p);
      obj.mu = mu;
      obj.Sigma = Sigma;
      obj.ComponentProportion = p;
      obj.NumComponents = size(mu, 1);
      obj.NumVariables = size(mu, 2);
    end
  end
end
%=============================================================================
