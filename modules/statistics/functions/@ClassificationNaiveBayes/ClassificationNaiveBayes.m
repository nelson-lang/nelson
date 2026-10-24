%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef ClassificationNaiveBayes
  properties
    ClassNames = []
    Prior = []
    Mu = zeros(0, 0)
    Sigma = zeros(0, 0)
    DistributionNames = {}
    DistributionParameters = {}
    NumObservations = 0
    NumPredictors = 0
  end

  methods
    function obj = ClassificationNaiveBayes(varargin)
      if nargin == 0
        return
      end
      error('Nelson:statistics:classificationNaiveBayesObjectsCreatedFitcnb', '%s objects are created by %s.', 'ClassificationNaiveBayes', 'fitcnb');
    end
  end
end
%=============================================================================
