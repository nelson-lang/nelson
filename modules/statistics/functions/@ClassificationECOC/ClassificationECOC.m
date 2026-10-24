%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef ClassificationECOC
  properties
    ClassNames = []
    BinaryLearners = {}
    CodingMatrix = []
    NumObservations = 0
    NumPredictors = 0
    LearnerOptions = struct()
  end

  methods
    function obj = ClassificationECOC(varargin)
      if nargin == 0
        return
      end
      error('Nelson:statistics:classificationECOCObjectsCreatedFitcecoc', '%s objects are created by %s.', 'ClassificationECOC', 'fitcecoc');
    end
  end
end
%=============================================================================
