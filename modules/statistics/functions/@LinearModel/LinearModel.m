%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef LinearModel
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
    SST = NaN
    SSR = NaN
    FStat = struct('F', NaN, 'pValue', NaN, 'DF1', NaN, 'DF2', NaN)
    ModelCriterion = struct('AIC', NaN, 'BIC', NaN)
    Intercept = true
    Terms = zeros(0, 0)
    Beta = []
    Covariance = []
  end

  methods
    function obj = LinearModel(varargin)
      if nargin == 0
        return
      end
      error('Nelson:statistics:linearModelObjectsCreatedFitlm', '%s objects are created by %s.', 'LinearModel', 'fitlm');
    end
  end
end
%=============================================================================
