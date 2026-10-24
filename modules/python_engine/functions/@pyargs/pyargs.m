%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef pyargs
  properties (Hidden)
    Arguments
  end

  methods
    function obj = pyargs(varargin)
      nargoutchk(0, 1);
      if (mod(nargin, 2) ~= 0)
        error('Nelson:pyargs:nameValueExpected', 'Name-value arguments expected.');
      end
      try
        obj.Arguments = struct(varargin{:});
      catch ex
        error('Nelson:python_engine:invalidFieldName', 'Field names must be string scalars or character vectors.');
      end
    end

    r = fieldnames(varargin)
    varargout = subsref(varargin)
    varargout = subsasgn(varargin)
    r = horzcat(varargin)
    r = vertcat(varargin)

    function n = numel(varargin)
      n = 1;
    end

    function n = length(varargin)
      n = 1;
    end

    function varargout = size(varargin)
      sz = [1 1];
      if nargin == 1
        if nargout <= 1
          varargout{1} = sz;
        else
          for k = 1:nargout
            if k <= length(sz)
              varargout{k} = sz(k);
            else
              varargout{k} = 1;
            end
          end
        end
      else
        dim = varargin{2};
        if dim <= length(sz)
          varargout{1} = sz(dim);
        else
          varargout{1} = 1;
        end
      end
    end
  end
end
%=============================================================================
