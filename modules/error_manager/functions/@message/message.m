%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef message
  % Build a message from its identifier and optional arguments. The message
  % text is resolved, on demand, from the identifier-keyed error catalog in the
  % current language.
  %
  %   m = message('Nelson:module:mnemonic', arg1, arg2);
  %   str = m.getString();
  %   error(m);
  properties
    Identifier
    Arguments
  end
  methods
    function obj = message(identifier, varargin)
      narginchk(1, inf);
      if ~(ischar(identifier) || (isstring(identifier) && isscalar(identifier)))
        error('Nelson:message:invalidIdentifier', 'Identifier must be a character vector or string scalar.');
      end
      obj.Identifier = char(identifier);
      obj.Arguments = varargin;
    end
    %=============================================================================
    function str = getString(obj)
      str = getUnformattedString(obj);
      for k = 1:numel(obj.Arguments)
        argument = obj.Arguments{k};
        if ischar(argument)
          replacement = argument;
        elseif isstring(argument) && isscalar(argument)
          replacement = char(argument);
        else
          replacement = num2str(argument);
        end
        candidates = [strfind(str, '%s'), strfind(str, '%d')];
        if isempty(candidates)
          break;
        end
        position = min(candidates);
        str = [str(1:position - 1), replacement, str(position + 2:end)];
      end
    end
    %=============================================================================
    function str = getUnformattedString(obj)
      str = char(i18nHelpers('template', obj.Identifier));
      if isempty(str)
        % No single catalog template for this identifier (generic/overloaded or
        % unknown): fall back to the identifier, consistent with error(message(...)).
        str = obj.Identifier;
      end
    end
    %=============================================================================
    function str = string(obj)
      str = getString(obj);
    end
  end
end
%=============================================================================
