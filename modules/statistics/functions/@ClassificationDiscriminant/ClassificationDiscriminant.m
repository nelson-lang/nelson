%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef ClassificationDiscriminant
  properties
    ClassNames = []
    Prior = []
    Mu = zeros(0, 0)
    Sigma = zeros(0, 0, 0)
    DiscrimType = 'linear'
    Gamma = 0
    Delta = 0
    NumObservations = 0
    NumPredictors = 0
  end

  methods
    function obj = ClassificationDiscriminant(varargin)
      if nargin == 0
        return
      end
      error('Nelson:statistics:classificationDiscriminantObjectsCreatedFitcdiscr', '%s objects are created by %s.', 'ClassificationDiscriminant', 'fitcdiscr');
    end
  end
end
%=============================================================================
