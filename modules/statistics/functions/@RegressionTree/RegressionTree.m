%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef RegressionTree
  properties
    CutPredictor = []
    CutPoint = []
    LeftChild = []
    RightChild = []
    NodeMean = []
    NodeError = []
    NodeSize = []
    IsBranch = []
    MaxNumSplits = 0
    MinLeafSize = 1
    MinParentSize = 10
    PredictorNames = {}
    ResponseName = 'y'
    NumObservations = 0
    NumPredictors = 0
    ResubstitutionLoss = NaN
  end

  methods
    function obj = RegressionTree(varargin)
      if nargin == 0
        return
      end
      error('Nelson:statistics:regressionTreeObjectsCreatedFitrtree', '%s objects are created by %s.', 'RegressionTree', 'fitrtree');
    end
  end
end
%=============================================================================
