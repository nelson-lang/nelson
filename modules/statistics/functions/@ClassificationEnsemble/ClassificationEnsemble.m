%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef ClassificationEnsemble
  properties
    ClassNames = []
    Learners = {}
    Method = 'Bag'
    NumLearningCycles = 100
    LearnerTemplate = 'tree'
    NumObservations = 0
    NumPredictors = 0
  end

  methods
    function obj = ClassificationEnsemble(varargin)
      if nargin == 0
        return
      end
      error('Nelson:statistics:classificationEnsembleObjectsCreatedFitcensemble', '%s objects are created by %s.', 'ClassificationEnsemble', 'fitcensemble');
    end
  end
end
%=============================================================================
