%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef RegressionSVM
  properties
    SupportVectors = zeros(0, 0)
    Alpha = []
    Bias = 0
    KernelFunction = 'linear'
    KernelScale = 1
    PolynomialOrder = 3
    BoxConstraint = 1
    Epsilon = 0.1
    Standardize = false
    Mu = []
    Sigma = []
    PredictorNames = {}
    ResponseName = 'y'
    NumObservations = 0
    NumPredictors = 0
  end

  methods
    function obj = RegressionSVM(varargin)
      if nargin == 0
        return
      end
      error('Nelson:statistics:regressionSVMObjectsCreatedFitrsvm', '%s objects are created by %s.', 'RegressionSVM', 'fitrsvm');
    end
  end
end
%=============================================================================
