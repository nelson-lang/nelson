%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef ClassdefStringCatHandleMethod < handle
  methods
    function obj = ClassdefStringCatHandleMethod(x)
      if nargin > 0
        error('ClassdefStringCatHandleMethod:converted', 'converted %s', class(x));
      end
    end
    function r = horzcat(varargin)
      r = ['horzcat:', strjoin(cellfun(@class, varargin, 'UniformOutput', false), ':')];
    end
  end
end
%=============================================================================
