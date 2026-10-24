%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef DerivedOptions < nelson.classdefpkg.Options
  properties
    ChildValue = 0
    SetterIsaValue = false
    LocalIsaValue = false
  end

  methods
    function obj = DerivedOptions(value)
      if nargin > 0
        obj.ChildValue = value;
      end
    end

    function obj = set.ChildValue(obj, value)
      obj.SetterIsaValue = isa(obj, 'nelson.classdefpkg.Options');
      obj.LocalIsaValue = localIsBaseOptions(obj);
      obj.ChildValue = value;
    end

    function tf = methodLocalIsa(obj)
      tf = localIsBaseOptions(obj);
    end
  end
end

function tf = localIsBaseOptions(obj)
  tf = isa(obj, 'nelson.classdefpkg.Options');
end
