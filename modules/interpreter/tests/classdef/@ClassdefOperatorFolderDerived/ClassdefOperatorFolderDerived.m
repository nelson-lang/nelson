%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef ClassdefOperatorFolderDerived < ClassdefOperatorFolderBase
  methods
    function obj = ClassdefOperatorFolderDerived(x)
      if nargin < 1
        x = 0;
      end
      obj = obj@ClassdefOperatorFolderBase(x);
    end
  end
end
%=============================================================================
