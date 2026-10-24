%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef (ConstructOnLoad, ...
    Hidden) ClassdefAdvancedAttributes < handle & nelson.mixin.Copyable
  properties (AbortSet, ...
      Transient, NonCopyable, GetObservable, SetObservable)
    Value(1, 1) ...
      double = 1
    Count(1, 1) double {mustBeInteger, mustBePositive} = 1
    Bounded(1, 1) double {mustBeGreaterThan(0), mustBeLessThanOrEqual(10)} = 5
    Choice(1, 1) double {mustBeMember([1, 3, 5])} = 1
  end

  events (ListenAccess = public, ...
      NotifyAccess = private)
    ValueChanged
  end

  methods
    function obj = ClassdefAdvancedAttributes(value)
      if nargin > 0
        obj.Value = value;
      end
    end
  end
end
