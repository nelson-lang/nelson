%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef ClassificationKNN
  properties
    X = zeros(0, 0)
    Y = []
    ClassNames = []
    NumNeighbors = 1
    Distance = 'euclidean'
    DistanceWeight = 'equal'
    Standardize = false
    Mu = []
    Sigma = []
    P = 2
    Scale = []
    NumObservations = 0
    NumPredictors = 0
  end

  methods
    function obj = ClassificationKNN(varargin)
      if nargin == 0
        return
      end
      error('Nelson:statistics:classificationKNNObjectsCreatedFitcknn', '%s objects are created by %s.', 'ClassificationKNN', 'fitcknn');
    end
  end
end
%=============================================================================
