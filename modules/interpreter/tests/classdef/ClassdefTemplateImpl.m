%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef (Sealed) ClassdefTemplateImpl < ClassdefTemplateBase
  % Implements the protected hooks of ClassdefTemplateBase.
  methods (Access = protected)
    function value = hook(obj, x)
      value = ['hook ', num2str(x)];
    end

    function value = step(obj)
      value = 'impl step';
    end
  end
end
%=============================================================================
