%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef ClassdefTemplateBase < handle
  % Base whose methods call protected hooks a subclass implements.
  methods (Abstract, Access = protected)
    value = hook(obj, x)
  end

  methods (Access = protected)
    function value = step(obj)
      value = 'base step';
    end
  end

  methods
    function value = callHook(obj, x)
      value = hook(obj, x);
    end

    function value = callHookDot(obj, x)
      value = obj.hook(x);
    end

    function value = callStep(obj)
      value = step(obj);
    end
  end

  methods (Hidden)
    function value = subsref(obj, s)
      value = hook(obj, s(1).subs);
    end

    function display(obj, varargin)
      fprintf('template display\n');
      disp(obj);
    end

    function disp(obj)
      fprintf('template disp %s\n', class(obj));
    end

    function varargout = horzcat(varargin) %#ok<STOUT>
      error('template:cat', 'no concatenation');
    end

    function varargout = vertcat(varargin) %#ok<STOUT>
      error('template:cat', 'no concatenation');
    end
  end
end
%=============================================================================
