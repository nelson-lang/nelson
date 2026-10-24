%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef ClassdefInlineBlocks < handle
  properties; A = 10; B = 20; end
  events; Changed; Updated; end
  methods
    function r = total(obj); r = obj.A + obj.B; end
  end
end
%=============================================================================
