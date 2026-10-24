%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef ClassdefInlineMethod
  methods (Static)
    function r = staticSemicolon(); r = 'static-semicolon'; end
    function r = staticComma(), r = 'static-comma', end
    function r = staticMulti(); a = 1; r = a + 2; end
    function r = staticNested(); if true; r = 'static-nested'; end; end
  end
  methods
    function r = instanceSemicolon(obj); r = 'instance-semicolon'; end
  end
end
%=============================================================================
