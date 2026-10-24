%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef ClassdefProtectedCtor
  properties
    Tag = ''
  end
  methods (Access = protected)
    function obj = ClassdefProtectedCtor()
    end
  end
  methods (Static)
    function obj = create(tag)
      obj = ClassdefProtectedCtor();
      obj.Tag = tag;
    end
  end
end
