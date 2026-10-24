%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef PropertyGroup
  % PropertyGroup  A titled group of properties for custom object display.
  properties
    Title = ''
    PropertyList = {}
  end
  properties (Dependent, SetAccess = private)
    NumProperties
  end
  methods
    function obj = PropertyGroup(propList, title)
      if nargin >= 1
        obj.PropertyList = propList;
      end
      if nargin >= 2
        obj.Title = title;
      end
    end
    function n = get.NumProperties(obj)
      n = numel(obj.PropertyList);
    end
  end
end
%=============================================================================
