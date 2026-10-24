%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef OcProbe < handle
  methods
    function instanceMethod(obj)
      c = onCleanup(@() disp('instance cleanup'));
      disp('instance body');
    end
    function failingMethod(obj)
      c = onCleanup(@() disp('failing cleanup'));
      error('OcProbe:failure', 'failure inside method');
    end
  end
  methods (Static)
    function staticMethod()
      c = onCleanup(@() disp('static cleanup'));
      disp('static body');
    end
  end
end
%=============================================================================
