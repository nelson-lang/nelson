%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef GeneralizedLinearModel
  properties
    Coefficients = table()
    Residuals = table()
    Fitted = []
    ResponseName = 'y'
    PredictorNames = {}
    CoefficientNames = {}
    NumObservations = 0
    NumPredictors = 0
    DFE = NaN
    RMSE = NaN
    Rsquared = struct('Ordinary', NaN, 'Adjusted', NaN)
    SSE = NaN
    Deviance = NaN
    Dispersion = NaN
    Distribution = 'normal'
    Link = 'identity'
    Intercept = true
    Terms = zeros(0, 0)
    Beta = []
    Covariance = []
    NumIterations = 0
    Converged = false
  end

  methods
    function obj = GeneralizedLinearModel(varargin)
      if nargin == 0
        return
      end
      error('Nelson:statistics:generalizedLinearModelObjectsCreatedFitglm', '%s objects are created by %s.', 'GeneralizedLinearModel', 'fitglm');
    end
  end
end
%=============================================================================
