%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef pattern
  properties (Hidden)
    Expression
    DisplayName
  end

  methods
    function obj = pattern(expression, displayName)
      if nargin == 0
        expression = '';
      end
      if nargin < 2
        displayName = '';
      end
      if isa(expression, 'pattern')
        obj.Expression = expression.Expression;
        obj.DisplayName = expression.DisplayName;
        return
      end
      if isstring(expression)
        expression = char(expression);
      end
      if isstring(displayName)
        displayName = char(displayName);
      end
      if ~ischar(expression) || ~ischar(displayName)
        error('Nelson:string:patternExpressionDisplayNameText', 'Pattern expression and display name must be text scalars.');
      end
      obj.Expression = expression;
      obj.DisplayName = displayName;
    end

    function out = plus(a, b)
      out = pattern(['(?:', pattern.regexOf(a), ')(?:', pattern.regexOf(b), ')']);
    end

    function out = or(a, b)
      out = pattern(['(?:', pattern.regexOf(a), ')|(?:', pattern.regexOf(b), ')']);
    end

    function out = not(a)
      out = pattern(['(?!(?:', pattern.regexOf(a), '))']);
    end

    function out = char(obj)
      out = obj.Expression;
    end

    function out = string(obj)
      out = string(obj.Expression);
    end

    function disp(obj)
      if isempty(obj.DisplayName)
        fprintf('pattern: %s\n', obj.Expression);
      else
        fprintf('pattern: %s\n', obj.DisplayName);
      end
    end

    function obj = subsasgn(obj, S, value)
      if numel(S) >= 1 && strcmp(S(1).type, '.')
        if numel(S) == 1 && strcmp(S(1).subs, 'Expression')
          obj.Expression = char(value);
        elseif numel(S) == 1 && strcmp(S(1).subs, 'DisplayName')
          obj.DisplayName = char(value);
        end
        return
      end
      error('Nelson:string:unsupportedPatternAssignment', 'Unsupported pattern assignment.');
    end

    function value = subsref(obj, S)
      if numel(S) >= 1 && strcmp(S(1).type, '.')
        if strcmp(S(1).subs, 'Expression')
          value = obj.Expression;
        elseif strcmp(S(1).subs, 'DisplayName')
          value = obj.DisplayName;
        else
          value = obj;
        end
        if numel(S) > 1
          value = subsref(value, S(2:end));
        end
        return
      end
      error('Nelson:string:unsupportedPatternIndexing', 'Unsupported pattern indexing.');
    end
  end

  methods (Static, Hidden)
    function expr = regexOf(value)
      if isa(value, 'pattern')
        expr = value.Expression;
      elseif ischar(value)
        expr = regexptranslate('escape', value);
      elseif isstring(value) && isscalar(value)
        expr = regexptranslate('escape', char(value));
      else
        error('Nelson:string:patternStringScalarCharacterVector', 'Pattern, string scalar, or character vector expected.');
      end
    end
  end
end
%=============================================================================
