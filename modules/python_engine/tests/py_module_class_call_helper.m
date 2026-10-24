%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [d, p, f] = py_module_class_call_helper()
  % class call (uppercase leaf) and function call (lowercase leaf) inside a
  % compiled function body
  d = py.collections.OrderedDict();
  p = py.os.path.join('a', 'b');
  % stdlib module function with a scalar return inside a compiled function body
  f = py.math.floor(2.5);
end
