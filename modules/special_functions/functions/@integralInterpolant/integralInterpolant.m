%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef integralInterpolant
  properties (Dependent)
    Integrand
    LowerLimit
    UpperLimit
    Integral
    ErrorBound
    AbsoluteTolerance
    RelativeTolerance
    Subintervals
  end

  properties (Hidden, SetAccess = private)
    Data = struct()
  end

  methods
    function obj = integralInterpolant(fun, lower, upper, varargin)
      narginchk(3, inf);
      obj.Data = __integral_interpolant__('build', fun, lower, upper, varargin{:});
    end

    function value = get.Integrand(obj)
      value = obj.Data.integrand;
    end

    function value = get.LowerLimit(obj)
      value = obj.Data.lower;
    end

    function value = get.UpperLimit(obj)
      value = obj.Data.upper;
    end

    function value = get.Integral(obj)
      value = obj.Data.integral;
    end

    function value = get.ErrorBound(obj)
      value = obj.Data.errbnd;
    end

    function value = get.AbsoluteTolerance(obj)
      value = obj.Data.absTol;
    end

    function value = get.RelativeTolerance(obj)
      value = obj.Data.relTol;
    end

    function value = get.Subintervals(obj)
      value = obj.Data.subintervals;
    end

    varargout = subsref(obj, s)
    disp(obj)
    display(obj)
  end
end
%=============================================================================
