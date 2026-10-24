%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef ClassificationTree
  properties
    ClassNames = []
    Prior = []
    CutPredictor = []
    CutPoint = []
    LeftChild = []
    RightChild = []
    NodeClass = []
    NodeProbability = []
    IsBranch = []
    SplitCriterion = 'gdi'
    MaxNumSplits = 0
    MinLeafSize = 1
    MinParentSize = 10
    NumObservations = 0
    NumPredictors = 0
  end

  methods
    function obj = ClassificationTree(varargin)
      if nargin == 0
        return
      end
      error('Nelson:statistics:classificationTreeObjectsCreatedFitctree', '%s objects are created by %s.', 'ClassificationTree', 'fitctree');
    end
  end
end
%=============================================================================
