%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef (Hidden) xmlNodeList
  properties (Hidden)
    Nodes = {}
  end

  methods
    function obj = xmlNodeList(nodes)
      if nargin > 0
        obj.Nodes = nodes;
      end
    end

    disp(varargin)
    display(varargin)
    n = getLength(nodes)
    node = item(nodes, index)
  end
end
%=============================================================================
