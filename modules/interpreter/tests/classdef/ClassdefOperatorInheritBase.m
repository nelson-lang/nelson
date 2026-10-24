%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef ClassdefOperatorInheritBase
  properties
    v = 0;
  end
  methods
    function r = plus(a, b)
      r = a.v + b.v;
    end
    function r = minus(a, b)
      r = a.v - b.v;
    end
    function r = mtimes(a, b)
      r = a.v * b.v;
    end
    function r = eq(a, b)
      r = a.v == b.v;
    end
    function r = horzcat(varargin)
      r = 'horzcat';
      for k = 1:nargin
        r = [r, ':', class(varargin{k})];
      end
    end
    function r = vertcat(varargin)
      r = 'vertcat';
      for k = 1:nargin
        r = [r, ':', class(varargin{k})];
      end
    end
  end
end
%=============================================================================
