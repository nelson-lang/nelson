%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef RegressionEnsemble
  properties
    Learners = {}
    Method = 'Bag'
    NumLearningCycles = 100
    LearnerTemplate = 'tree'
    PredictorNames = {}
    ResponseName = 'y'
    NumObservations = 0
    NumPredictors = 0
    ResubstitutionLoss = NaN
    LearnRate = 1
    LearnerWeights = []
    InitialResponse = 0
  end

  methods
    function obj = RegressionEnsemble(varargin)
      if nargin == 0
        return
      end
      error('Nelson:statistics:regressionEnsembleObjectsCreatedFitrensemble', '%s objects are created by %s.', 'RegressionEnsemble', 'fitrensemble');
    end
  end
end
%=============================================================================
