%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Regression: a class method whose signature declares 'varargout' must not
% error when it is called with no outputs requested (nargout == 0) and the
% body never assigns 'varargout'. The bytecode output path wrongly raised
% 'Nelson:interpreter:invalidVarargout' even though the identical pattern is
% accepted for a plain (non-method) function. Only varargout actually being
% consumed (nargout > named outputs) requires it to be a defined cell array.
%=============================================================================
path_test = [modulepath('interpreter', 'tests'), '/classdef'];
addpath(path_test);
%=============================================================================
o = ClassdefVarargoutUnset();
%=============================================================================
% nargout == 0 (statement form): unset varargout must NOT error.
o.doit();
%=============================================================================
% nargout == 0 via function-call form: also must NOT error.
doit(o);
%=============================================================================
% Requesting an output when varargout is unset must STILL error.
asserts.checkerror('r = o.doit();', ...
  _('The special variable ''varargout'' was not defined as a cell-array.'));
%=============================================================================
% A method that assigns varargout returns it when requested.
v = o.give();
asserts.isequal(v, 42);
%=============================================================================
% Parity: the identical pattern on a plain function already works (nargout 0).
tmp_varargout_unset_plain();
%=============================================================================

function [varargout] = tmp_varargout_unset_plain()
  % never assigns varargout; statement form (nargout == 0) must not error
  disp('plain ran');
end
%=============================================================================
