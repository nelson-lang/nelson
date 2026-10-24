%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Compiled-body caller: exercises static dispatch of a lowercase-named class
% from a function whose body is compiled before the class is loaded.
function r = call_lowercasestatichost(x)
  r = lowercasestatichost.scale(x);
end
%=============================================================================
