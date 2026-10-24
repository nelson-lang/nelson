%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef ClassdefStringCatRefuse
  methods
    function obj = ClassdefStringCatRefuse(x)
      if nargin > 0
        error('ClassdefStringCatRefuse:refused', 'refused %s', class(x));
      end
    end
  end
end
%=============================================================================
