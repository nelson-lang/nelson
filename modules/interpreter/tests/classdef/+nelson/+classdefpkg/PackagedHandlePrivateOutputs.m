%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef PackagedHandlePrivateOutputs < handle
  methods
    function [first, second] = publicCall(obj)
      [first, second] = obj.privateCall();
    end
  end

  methods (Access = private)
    function [first, second] = privateCall(obj)
      first = 11;
      second = 22;
    end
  end
end
