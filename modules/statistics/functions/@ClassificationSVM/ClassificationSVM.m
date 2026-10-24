%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef ClassificationSVM
  properties
    SupportVectors = zeros(0, 0)
    SupportVectorLabels = []
    Alpha = []
    Bias = 0
    ClassNames = []
    KernelFunction = 'linear'
    KernelScale = 1
    PolynomialOrder = 3
    BoxConstraint = 1
    BoxConstraints = []
    Cost = []
    Standardize = false
    Mu = []
    Sigma = []
    NumObservations = 0
    NumPredictors = 0
    PredictorNames = {}
    ConvergenceInfo = struct('Converged', false, 'Iterations', 0)
  end

  methods
    function obj = ClassificationSVM(varargin)
      if nargin == 0
        return
      end
      error('Nelson:statistics:classificationSVMObjectsCreatedFitcsvm', '%s objects are created by %s.', 'ClassificationSVM', 'fitcsvm');
    end
  end
end
%=============================================================================
