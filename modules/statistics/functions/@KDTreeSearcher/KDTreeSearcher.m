%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef KDTreeSearcher
  properties
    X = zeros(0, 0)
    Distance = 'euclidean'
    DistParameter = []
    BucketSize = 50
  end
  methods
    function obj = KDTreeSearcher(X, varargin)
      if nargin < 1
        return;
      end
      if ~(isnumeric(X) && isreal(X) && ismatrix(X) && ~issparse(X))
        error('Nelson:statistics:xRealNumericMatrix', '%s must be a real numeric matrix.', 'X');
      end
      obj.X = double(X);
      if mod(numel(varargin), 2) ~= 0
        error('Nelson:statistics:nameValuePairs', 'Name-value arguments must come in pairs.');
      end
      for k = 1:2:numel(varargin)
        name = varargin{k};
        value = varargin{k + 1};
        switch lower(name)
          case 'distance'
            obj.Distance = value;
          case 'p'
            obj.DistParameter = value;
          case 'bucketsize'
            obj.BucketSize = value;
          otherwise
            error('Nelson:statistics:unknownOption', 'Unknown option: %s.', name);
        end
      end
    end
  end
end
%=============================================================================
